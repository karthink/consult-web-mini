;;; cw.el --- multi-source async search with Consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Version: 0.2
;; Package-Requires: ((emacs "29.1") (consult "2.0") (plz "0.7"))
;; Homepage: https://github.com/karthink/
;; Keywords: convenience, completion

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Search the web and local databases at once.  This is a
;; multi-source completion UI built on `consult--multi' from Consult
;; (version 2.0 or later).  Sources that are unavailable disable
;; themselves automatically.

;; The Brave source requires a Brave Search API key, see
;; `cw-brave-api-key'.  The other sources require the gptel,
;; browser-hist, elfeed and wombag packages respectively.

;;; Code:
(require 'consult)
(require 'plz)
(require 'url-parse)
(require 'url-util)

(declare-function gptel "ext:gptel")
(declare-function gptel-request "ext:gptel")
(declare-function gptel-backend-name "ext:gptel")
(declare-function gptel-prompt-prefix-string "ext:gptel")
(declare-function gptel-send "ext:gptel")
(declare-function eww-readable "ext:eww")
(declare-function elfeed-show-entry "ext:elfeed-show")
(declare-function elfeed-search-parse-filter "ext:elfeed-search")
(declare-function elfeed-search-compile-filter "ext:elfeed-search")
(declare-function elfeed-search-format-date "ext:elfeed-search")
(declare-function elfeed-search--faces "ext:elfeed-search")
(declare-function elfeed-format-column "ext:elfeed-search")
(declare-function elfeed-clamp "ext:elfeed-search")
(declare-function elfeed-entry-date "ext:elfeed")
(declare-function elfeed-entry-title "ext:elfeed")
(declare-function elfeed-entry-feed "ext:elfeed")
(declare-function elfeed-entry-link "ext:elfeed")
(declare-function elfeed-entry-tags "ext:elfeed")
(declare-function elfeed-feed-title "ext:elfeed")
(declare-function elfeed-meta "ext:elfeed")
(declare-function elfeed-db-get-entry "ext:elfeed-db")
(declare-function elfeed-db-index "ext:elfeed-db")
(declare-function elfeed-db-ensure "ext:elfeed-db")
(declare-function elfeed-entry-feed "ext:elfeed-db")
(declare-function avl-tree-mapc "ext:avl-tree")
(declare-function with-elfeed-db-visit "ext:elfeed-db" (spec &rest body) nil t)
(declare-function wombag-search-parse-filter "ext:wombag-search")
(declare-function wombag-db-get-entries "ext:wombag-db")
(declare-function wombag-search-format-entry "ext:wombag-search")
(declare-function wombag-show-entry "ext:wombag")
(declare-function browser-hist--send-query "ext:browser-hist")
(declare-function notmuch-tree-format-field "ext:notmuch-tree")
(declare-function notmuch-tree "ext:notmuch-tree")
(declare-function notmuch-show "ext:notmuch-show")

(defvar gptel-model)
(defvar gptel-stream)
(defvar gptel-backend)
(defvar gptel-max-tokens)
(defvar gptel-use-curl)
(defvar elfeed-search-title-min-width)
(defvar elfeed-search-title-max-width)
(defvar wombag-search-columns)
(defvar notmuch-search-result-format)
(defvar notmuch-show-only-matching-messages)

;; For the `with-elfeed-db-visit' macro at compile time.
(eval-when-compile (require 'elfeed-db nil t))

;;; Convenience

(defvar cw--search-history nil
  "History variable for `cw-search' and co.")

(defvar cw--count 5
  "Max number of results per source.")

(defun cw--async (&rest builders)
  "Return an async source pipeline around curried async BUILDERS."
  (apply #'consult--async-pipeline
         (consult--async-min-input 3)
         (consult--async-throttle 0.5 0.4)
         builders))

;;; Commands

(defvar cw-source-elfeed)
(defvar cw-source-gptel)
(defvar cw-source-brave)
(defvar cw-source-wombag)
(defvar cw-source-browser-hist)
(defvar cw-source-notmuch)
(defvar cw-source-recoll)

(defun cw-search ()
  "Search elfeed, wallabag, browser history, Brave and gptel at once."
  (interactive)
  (consult--multi
   (list cw-source-elfeed cw-source-gptel cw-source-brave cw-source-recoll
         cw-source-wombag cw-source-browser-hist cw-source-notmuch)
   :prompt "Search: " :sort nil
   :category 'consult-web
   :history 'cw--search-history))

(defun cw-search-local ()
  "Search local sources: elfeed, wallabag and browser history."
  (interactive)
  (consult--multi
   (list cw-source-elfeed cw-source-wombag cw-source-recoll
         cw-source-browser-hist cw-source-notmuch)
   :prompt "Search (local sources): " :sort nil
   :category 'consult-web
   :history 'cw--search-history))

;;; Sources

;;;; Notmuch

(defcustom cw-notmuch-search-single t
  "Whether to search for threads or single messages.
If true, search at the message level, otherwise return email threads."
  :type 'boolean
  :group 'cw)

(defvar cw--notmuch-buffer "*cw-notmuch*"
  "Buffer name for notmuch previews.")

;; TODO Use a closure and avoid global vars?
(defvar cw--notmuch-partial-parse nil
  "Internal variable for parsing status.")
(defvar cw--notmuch-partial-headers nil
  "Internal variable for parsing status.")
(defvar cw--notmuch-info nil
  "Internal variable for parsing status.")

(defsubst cw--notmuch-set (k v)
  "Set the value V for property K in the message we're currently parsing."
  (setq cw--notmuch-partial-parse
        (plist-put cw--notmuch-partial-parse k v)))

(defsubst cw--notmuch-candidate-id (cand)
  "Get the CAND ID for the email message."
  (and cand (get-text-property 0 'id cand)))

(defun cw--notmuch-command-args (input)
  "Arguments for calling Notmuch on INPUT."
  (if cw-notmuch-search-single
      (list "notmuch" "show" "--body=false"
            (format "--limit=%d" cw--count) "--sort=newest-first" input)
    (list "notmuch" "search" "--sort=newest-first"
          (format "--limit=%d" cw--count) input)))

(defun cw--notmuch-transformer (str)
  "Format Notmuch result STR for Consult."
  (if cw-notmuch-search-single
      (cw--notmuch-show-transformer str)
    (cw--notmuch-search-transformer str)))

(defun cw--notmuch-search-transformer (str)
  "Transform STR from notmuch search to notmuch display style."
  (when (string-match "thread:" str)
    (let* ((id (car (split-string str "\\ +")))
           (date (substring str 24 37))
           (mid (substring str 24))
           (c0 (string-match "[[]" mid))
           (c1 (string-match "[]]" mid))
           (count (substring mid c0 (1+ c1)))
           (auths (string-trim (nth 1 (split-string mid "[];]"))))
           (subject (string-trim (nth 1 (split-string mid "[;]"))))
           (headers (list :Subject subject :From auths))
           (t0 (string-match "([^)]*)\\s-*$" mid))
           (tags (split-string (substring mid (1+  t0) -1)))
           (msg (list :id id
                      :match t
                      :headers headers
                      :count count
                      :date_relative date
                      :tags tags)))
      (cw--notmuch-format-candidate msg))))

(defun cw--notmuch-show-transformer (str)
  "Parse output STR of notmuch show, extracting its components."
  (if (string-prefix-p "message}" str)
      (prog1
          (cw--notmuch-format-candidate
           (cw--notmuch-set :headers cw--notmuch-partial-headers))
        (setq cw--notmuch-partial-parse nil
              cw--notmuch-partial-headers nil
              cw--notmuch-info nil))
    (cond ((string-match "message{ \\(id:[^ ]+\\) .+" str)
           (cw--notmuch-set :id (match-string 1 str))
           (cw--notmuch-set :match t))
          ((string-prefix-p "header{" str)
           (setq cw--notmuch-info t))
          ((and str cw--notmuch-info)
           (when (string-match "\\(.+\\) (\\([^)]+\\)) (\\([^)]*\\))$" str)
             (cw--notmuch-set :Subject (match-string 1 str))
             (cw--notmuch-set :date_relative (match-string 2 str))
             (cw--notmuch-set :tags (split-string (match-string 3 str))))
           (setq cw--notmuch-info nil))
          ((string-match "\\(Subject\\|From\\|To\\|Cc\\|Date\\): \\(.+\\)?" str)
           (let ((k (intern (format ":%s" (match-string 1 str))))
                 (v (or (match-string 2 str) "")))
             (setq cw--notmuch-partial-headers
                   (plist-put cw--notmuch-partial-headers k v)))))
    nil))

(defun cw--notmuch-format-candidate (msg)
  "Format the result (MSG) of parsing a notmuch show information unit."
  (when-let* ((id (plist-get msg :id)))
    (let ((result-string))
      (dolist (spec notmuch-search-result-format)
        (when-let* ((field (cw--notmuch-format-field spec msg)))
          (setq result-string (concat result-string field))))
      (propertize result-string 'id id 'tags (plist-get msg :tags)))))

(defun cw--notmuch-format-field (spec msg)
  "Return a string for SPEC given the MSG metadata."
  (let ((field (car spec)))
    (cond ((equal field "count")
           (when-let* ((cnt (plist-get msg :count)))
             (format (cdr spec) cnt)))
          ((equal field "tags")
           (when (plist-get msg :tags)
             (notmuch-tree-format-field "tags" (cdr spec) msg)))
          (t (notmuch-tree-format-field field (cdr spec) msg)))))

(defun cw--notmuch-state ()
  "Preview and action function for Notmuch results."
  (lambda (action cand)
    (pcase action
      ('preview
       (when-let* ((id (cw--notmuch-candidate-id cand)))
         (when (get-buffer cw--notmuch-buffer)
           (kill-buffer cw--notmuch-buffer))
         (let ((notmuch-show-only-matching-messages nil))
           (notmuch-show id nil nil nil cw--notmuch-buffer))))
      ('return
       (when-let* ((thread-id (cw--notmuch-candidate-id cand)))
         (when (get-buffer cw--notmuch-buffer)
           (kill-buffer cw--notmuch-buffer))
         (notmuch-tree thread-id nil nil nil)))
      ('exit
       (when (get-buffer cw--notmuch-buffer)
         (kill-buffer cw--notmuch-buffer))))))

(defvar cw-source-notmuch
  `(:name     "Notmuch"
    :narrow   ?m
    :category 'consult-web
    :preview-key "M-RET"
    :face     font-lock-operator-face
    :state    ,#'cw--notmuch-state
    :enabled  ,(lambda () (fboundp 'notmuch-search))
    :async    ,(cw--async
                (consult--process-collection #'cw--notmuch-command-args)
                (consult--async-map #'cw--notmuch-transformer)
                (consult--async-filter #'identity))))

;;;; Recoll
(defvar cw-source-recoll
  `( :name     "Recoll"
     :narrow   ?f
     :category 'consult-web
     :preview-key any
     :face     font-lock-constant-face
     :annotate (lambda (cand) (propertize (get-text-property 0 'url cand)
                                     'face 'font-lock-comment-face))
     :state    ,#'cw--recoll-state
     :enabled  ,(lambda () (executable-find "recollq"))
     :async    ,(cw--async
                 (consult--process-collection #'cw--recoll-command)
                 (consult--async-map #'cw--recoll-transformer)
                 (consult--async-filter #'identity))))

(defvar cw--recoll-current nil)
(defvar cw--recoll-index 0)
(defvar cw--recoll-snippets nil)

(defun cw--recoll-command (text)
  "Command used to perform queries for TEXT."
  (setq cw--recoll-current nil)
  (setq cw--recoll-index 0)
  (setq cw--recoll-snippets nil)
  ;; See recollq for flags.  Main ones:
  ;; -A:  include snippets
  ;; -p5: include page numbers, 5 snippets per file
  ;; -a:  and-compose terms
  ;; -o:  or-compose terms
  ;; -n100: Print max 100 results
  `("recollq" "-A" "-p" "5" "-a" ,text))

(defconst cw--recoll-line-regex
  "^\\(.*?\\)\t\\[\\(.*?\\)\\]\t\\[\\(.*\\)\\]\\(\t\\([0-9]+\\)\\)?"
  "Regular expression decomposing result lines returned by recollq.")

(defvar cw-recoll-inline-snippets nil
  "Whether snippets from files should be shown inline.")

(defun cw--recoll-transformer (str)
  "Decode STR, as returned by recollq."
  (cond ((string-match-p "^/?SNIPPETS$" str) nil)
        ((string-match cw--recoll-line-regex str)
         (let* ((mime (match-string 1 str))
                (url (match-string 2 str))
                (title (match-string 3 str))
                (size (match-string 5 str))
                (urln (if (string-prefix-p "file://" url)
                          (abbreviate-file-name (substring url 7)) url))
                (idx (setq cw--recoll-index (1+ cw--recoll-index)))
                ;; (cand (consult-recoll--format title url mime))
                (cand (propertize title
                                  'mime-type mime
                                  'url urln
                                  'title title
                                  'index idx
                                  'size size)))
           (push () cw--recoll-snippets)
           (setq cw--recoll-current cand)))
        ((and cw-recoll-inline-snippets cw--recoll-current)
         (when-let* ((page (and (string-match "^\\([0-9]+\\) :" str)
                                (match-string 1 str)))
                     (pageno (and page (string-to-number page)))
                     (props (text-properties-at 0 cw--recoll-current)))
           (apply #'propertize (concat "    " (propertize str 'face 'shadow))
                  'page pageno props)))
        (cw--recoll-current
         (push str (car cw--recoll-snippets))
         nil)))

(defsubst cw--recoll-snippets (candidate)
  "Combine snippets for CANDIDATE."
  (let* ((len (length cw--recoll-snippets))
         (idx (or (get-text-property 0 'index candidate) 0))
         (pos (- len idx)))
    (if (>= pos len)
        ""
      (mapconcat 'identity (reverse (elt cw--recoll-snippets pos)) "\n"))))

(declare-function embark-open-externally "ext:embark")

(defun cw--recoll-state ()
  "Recoll result preview function."
  (lambda (action candidate)
    "Preview search result CANDIDATE when ACTION is \\='preview."
    (cond ((or (eq action 'setup) (null candidate))
           (with-current-buffer (get-buffer-create "*cw-recoll-preview*")
             (setq-local cursor-in-non-selected-windows nil)
             (delete-region (point-min) (point-max))))
          ((and (eq action 'preview) candidate)
           (when-let* ((url (get-text-property 0 'url candidate))
                       (buff (get-buffer "*cw-recoll-preview*")))
             (with-current-buffer buff
               (delete-region (point-min) (point-max))
               (when-let* ((title (get-text-property 0 'title candidate)))
                 (insert (propertize title 'face 'consult-recoll-title-face) "\n"))
               (insert (propertize url 'face 'consult-recoll-url-face) "\n")
               (insert (propertize (get-text-property 0 'mime-type candidate)
                                   'face 'consult-recoll-mime-face))
               (when-let* ((s (cw--recoll-snippets candidate)))
                 (insert "\n\n" s))
               (goto-char (point-min)))
             (pop-to-buffer buff '((display-buffer-reuse-window
                                    display-buffer-at-bottom
                                    display-buffer-in-side-window)
                                   (side . bottom)
                                   (window-parameters . ((mode-line-format . none)))
                                   (window-height . fit-window-to-buffer)))))
          ((and (eq action 'return) candidate)
           (if-let* ((url (get-text-property 0 'url candidate))
                     ((file-exists-p url)))
               (embark-open-externally url)
             (message "File %s is not readable!" url)))
          ((eq action 'exit)
           (when (get-buffer "*cw-recoll-preview*")
             (kill-buffer "*cw-recoll-preview*"))))))

;;;; gptel

(defun cw--gptel-state ()
  "gptel result preview function."
  (let ((query-sent) (buffer-preview (consult--buffer-preview)))
    (lambda (action cand)
      (pcase action
        ('exit (funcall buffer-preview 'exit cand))
        ((or 'preview 'return)
         (if cand
             (let* ((props (text-properties-at 0 cand))
                    (query (plist-get props :query))
                    (gptel-buffer (gptel "*cw-gptel*")))
               (with-current-buffer gptel-buffer
                 (unless query-sent
                   (erase-buffer)
                   (insert (gptel-prompt-prefix-string) query)
                   (setq query-sent t)
                   (gptel-send)))
               (funcall buffer-preview 'preview gptel-buffer))
           (funcall buffer-preview 'preview cand)))))))

(defun cw--gptel-async ()
  "Return async builder for gptel search.
Queries gptel for a short response to the input.  A generation
counter discards responses to stale queries."
  (lambda (sink)
    (let ((generation 0))
      (lambda (action)
        (pcase action
          ((pred stringp)
           (funcall sink 'flush)
           (let ((gen (setq generation (1+ generation)))
                 (gptel-max-tokens 24)
                 (gptel-use-curl nil))
             (gptel-request action
               :system "Respond in 10 words or less."
               :callback
               (lambda (response _)
                 (when (and (eq gen generation) (stringp response))
                   (funcall sink
                            (list
                             (propertize (string-trim-right response)
                                 :title response
                                 :query action
                                 :model gptel-model
                                 :stream gptel-stream
                                 :backend (gptel-backend-name gptel-backend)))))))))
          ('destroy
           (setq generation (1+ generation))
           (funcall sink action))
          (_ (funcall sink action)))))))

(defun cw-gptel-annotate (cand)
  "Annotate gptel candidate CAND with its backend and model."
  (let* ((props (text-properties-at 0 cand))
         (model (plist-get props :model))
         (stream (plist-get props :stream))
         (backend (plist-get props :backend)))
    (concat " " (propertize " " 'display '(space :align-to center))
            (propertize backend 'face 'font-lock-variable-name-face)
            (propertize (format ":%s" model) 'face 'font-lock-warning-face)
            (and stream (propertize " ~stream~ " 'face 'font-lock-comment-face)))))

(defvar cw-source-gptel
  `(:name     "gptel"
    :narrow   ?g
    :category 'consult-web
    :preview-key "M-RET"
    :face     font-lock-operator-face
    :annotate ,#'cw-gptel-annotate
    :state    ,#'cw--gptel-state
    :enabled  ,(lambda () (fboundp 'gptel))
    :async    ,(cw--async (cw--gptel-async))))

;;;; Brave

(defun cw--eww-readable-once ()
  "Call `eww-readable', then remove this function from `eww-after-render-hook'."
  (eww-readable)
  (remove-hook 'eww-after-render-hook #'cw--eww-readable-once))

(defun cw--brave-state ()
  "Preview Brave results in EWW, open in browser on selection."
  (let ((buffer-preview (consult--buffer-preview)))
    (lambda (action cand)
      (pcase action
        ('exit (funcall buffer-preview 'exit cand))
        ((or 'preview 'return)
         (if cand
             (when-let* ((props (text-properties-at 0 cand))
                         (url (or (plist-get props :url)
                                  (plist-get props :search-url))))
               (if (eq action 'preview)
                   (progn
                     (add-hook 'eww-after-render-hook #'cw--eww-readable-once)
                     (funcall buffer-preview 'preview (eww-browse-url url)))
                 (browse-url url)))
           (funcall buffer-preview 'preview cand)))))))

(defvar cw-brave-url "https://api.search.brave.com/res/v1/web/search"
  "Brave web search API endpoint.")

(defvar cw-brave-api-key nil
  "Brave Search API key, or a function returning it.")

(defun cw-brave-url-string (query)
  "Return the Brave API URL searching for QUERY."
  (concat cw-brave-url "?"
          (url-build-query-string
           `(("q" ,query)
             ("count" ,(number-to-string cw--count))
             ("page" "0")))))

(defun cw--brave-format (item query)
  "Format Brave search result ITEM as a candidate string for QUERY."
  (let* ((title (map-elt item :title))
         (url (map-elt item :url))
         (urlobj (and url (url-generic-parse-url url)))
         (domain (and (url-p urlobj) (url-domain urlobj)))
         (domain (and (stringp domain)
                      (propertize domain 'face 'font-lock-variable-name-face)))
         (path (and (url-p urlobj) (url-filename urlobj)))
         (path (and (stringp path)
                    (propertize path 'face 'font-lock-warning-face)))
         (decorated (concat title "\t"
                            (propertize " " 'display '(space :align-to center))
                            domain path)))
    (propertize decorated
                :title title
                :url url
                :search-url (cw-brave-url-string query)
                :query query)))

(defun cw--brave-async ()
  "Return async builder for Brave search.
A generation counter discards responses to stale queries."
  (lambda (sink)
    (let ((generation 0))
      (lambda (action)
        (pcase action
          ((pred stringp)
           (funcall sink 'flush)
           (let ((gen (setq generation (1+ generation)))
                 (query action))
             (plz 'get (cw-brave-url-string query)
               :headers `(("Accept" . "application/json")
                          ("X-Subscription-Token"
                           . ,(if (functionp cw-brave-api-key)
                                  (funcall cw-brave-api-key)
                                cw-brave-api-key)))
               :as (lambda ()
                     (condition-case nil
                         (json-parse-buffer :object-type 'plist)
                       (error nil)))
               :then (lambda (attrs)
                       (when-let* (((eq gen generation))
                                   (results (map-nested-elt attrs '(:web :results))))
                         (funcall sink
                                  (mapcar (lambda (item)
                                            (cw--brave-format item query))
                                          results))))
               :else (lambda (err)
                       (message "cw: Brave search failed: %S" err)))))
          ('destroy
           (setq generation (1+ generation))
           (funcall sink action))
          (_ (funcall sink action)))))))

(defvar cw-source-brave
  `(:name     "Brave"
    :narrow   ?b
    :category consult-web
    :preview-key "M-RET"
    :state    ,#'cw--brave-state
    :enabled  ,(lambda () cw-brave-api-key)
    :async    ,(cw--async (cw--brave-async))))

;;;; Elfeed

(defun cw--elfeed-state ()
  "Elfeed entry preview function."
  (let ((buffer-preview (consult--buffer-preview)))
    (lambda (action cand)
      (pcase action
        ('exit (funcall buffer-preview 'exit cand))
        ((or 'preview 'return)
         (if cand
             (when-let* ((entry (get-text-property 0 :entry cand))
                         (buf (elfeed-show-entry entry)))
               (funcall buffer-preview 'preview buf))
           (funcall buffer-preview 'preview nil)))))))

(defun cw--elfeed-search (query)
  "Return elfeed entries matching QUERY as candidate strings."
  (let* ((elfeed-search-filter (concat (format "#%d " cw--count) query))
         (filter (elfeed-search-parse-filter elfeed-search-filter))
         (head (list nil)) (tail head) (count 0)
         ;; Bind `lexical-binding' for `byte-compile' below: the filter
         ;; closure must capture its variables lexically.
         (lexical-binding t)
         (search-func (byte-compile (elfeed-search-compile-filter filter))))
    (with-elfeed-db-visit (entry feed)
      (when (funcall search-func entry feed count)
        (setf (cdr tail) (list entry)
              tail (cdr tail)
              count (1+ count))))
    (when-let ((entries (cdr head)))
      (cw-elfeed-annotate entries))))

(defun cw-elfeed-annotate (entries)
  "Return annotated candidate strings for elfeed ENTRIES."
  (let ((annotated-entries))
    (dolist (entry entries annotated-entries)
      (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
             (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
             (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
             (feed (elfeed-entry-feed entry))
             (feed-title
              (when feed
                (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
             (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
             (tags-str (mapconcat
                        (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                        tags ","))
             (title-width 60)
             (title-column (elfeed-format-column
                            title (elfeed-clamp
                                   elfeed-search-title-min-width
                                   title-width
                                   elfeed-search-title-max-width)
                            :left)))
        (push (propertize
               (concat (propertize date 'face 'elfeed-search-date-face) " "
                       (propertize title-column 'face title-faces 'kbd-help title)
                       (when feed-title
                         (concat (propertize feed-title 'face 'elfeed-search-feed-face) " "))
                       (when tags (concat "(" tags-str ")")))
               :entry entry
               :url (elfeed-entry-link entry))
              annotated-entries)))))

(defvar cw-source-elfeed
  `(:name     "Elfeed"
    :narrow   ?e
    :category 'consult-web
    :preview-key "M-RET"
    :state    ,#'cw--elfeed-state
    :enabled  ,(lambda () (boundp 'elfeed-db))
    :async    ,(cw--async (consult--async-dynamic #'cw--elfeed-search))))

;;;; Wombag (wallabag)

(defun cw--wombag-search (query)
  "Return wallabag entries matching QUERY as candidate strings."
  ;; Strip text properties (consult's splitter adds `consult--force'):
  ;; emacsql escapes SQL values with `prin1-to-string', so a
  ;; propertized string would end up in the SQL as a `#(...)' read
  ;; syntax blob and match nothing.
  (setq query (substring-no-properties query))
  (let* ((wombag-search-filter (concat (format "#%d " cw--count) query))
         (filter (wombag-search-parse-filter
                  wombag-search-filter wombag-search-columns))
         (entries (wombag-db-get-entries filter wombag-search-columns)))
    (when entries
      (mapcar (lambda (entry) (propertize (wombag-search-format-entry entry)
                                     :entry entry
                                     :url (alist-get 'url entry)))
              entries))))

(defun cw--wombag-state ()
  "Wallabag entry preview function."
  (let ((buffer-preview (consult--buffer-preview)))
    (lambda (action cand)
      (pcase action
        ('exit (funcall buffer-preview 'exit cand))
        ((or 'preview 'return)
         (if cand
             (when-let* ((entry (get-text-property 0 :entry cand))
                         (buf (wombag-show-entry entry)))
               (funcall buffer-preview 'preview buf))
           (funcall buffer-preview 'preview nil)))))))

(defvar cw-source-wombag
  `(:name     "Wallabag"
    :narrow   ?w
    :category 'consult-web
    :preview-key "M-RET"
    :state    ,#'cw--wombag-state
    :enabled  ,(lambda () (featurep 'wombag-search))
    :async    ,(cw--async (consult--async-dynamic #'cw--wombag-search))))

;;;; Browser history

(defun cw--browser-hist-search (query)
  "Return browser history entries matching QUERY as candidates."
  (when (require 'browser-hist nil t)
    (when-let ((results (browser-hist--send-query query)))
      (mapcar (pcase-lambda (`(,url . ,title))
                (let* ((urlobj (and url (url-generic-parse-url url)))
                       (domain (and (url-p urlobj) (url-domain urlobj)))
                       (domain (and (stringp domain)
                                    (propertize domain 'face 'font-lock-variable-name-face)))
                       (path (and (url-p urlobj) (url-filename urlobj)))
                       (path (and (stringp path)
                                  (propertize path 'face 'font-lock-warning-face)))
                       (decorated (concat
                                   (truncate-string-to-width (or title url) (floor (window-width) 2))
                                   "\t"
                                   (propertize " " 'display '(space :align-to center))
                                   domain path)))
                  (propertize decorated
                              :title title
                              :url url
                              :query query)))
              results))))

(defvar cw-source-browser-hist
  `(:name     "Browser history"
    :narrow   ?h
    :category 'consult-web
    :preview-key "M-RET"
    :state    ,#'cw--brave-state
    :enabled  ,(lambda () (fboundp 'browser-hist-search))
    :async    ,(cw--async (consult--async-dynamic #'cw--browser-hist-search))))

;;;; Invidious

(defvar cw-source-invidious
  `(:name     "Youtube"
    :narrow   ?y
    :category 'consult-web
    :preview-key "M-RET"
    :state    ,#'cw--brave-state
    :hidden   t))

(defface cw--invidious-published-face
  '((((class color) (background light)) (:foreground "#a0a"))
    (((class color) (background dark))  (:foreground "#7a7")))
  "Face used for the video published date.")

(defface cw--invidious-author-face
  '((((class color) (background light)) (:foreground "#aa0"))
    (((class color) (background dark))  (:foreground "#ff0")))
  "Face used for channel names.")

(defface cw--invidious-length-face
  '((((class color) (background light)) (:foreground "#aaa"))
    (((class color) (background dark))  (:foreground "#77a")))
  "Face used for the video length.")

(defvar cw--invidious-servers nil
  "List of Invidious instances with a working API.")

(defun cw--invidious-state ()
  "Invidious result preview function."
  (let ((buffer-preview (consult--buffer-preview)))
    (lambda (action cand)
      (pcase action
        ('exit (funcall buffer-preview 'exit cand))
        ((or 'preview 'return)
         (if cand
             (when-let* ((props (text-properties-at 0 cand))
                         (url (or (plist-get props :url)
                                  (plist-get props :search-url))))
               (if (eq action 'preview)
                   (progn
                     (add-hook 'eww-after-render-hook #'cw--eww-readable-once)
                     (funcall buffer-preview 'preview (eww-browse-url url)))
                 (browse-url url)))
           (funcall buffer-preview 'preview cand)))))))

(defun cw--get-invidious-servers (&optional rotate)
  "Return the list of Invidious servers, ROTATE it if requested."
  (when (and cw--invidious-servers rotate)
    (setq cw--invidious-servers
          (nconc (cdr cw--invidious-servers)
                 (list (car cw--invidious-servers)))))
  (or cw--invidious-servers
      (setq cw--invidious-servers
            (when-let ((raw
                        (plz 'get (concat "https://api.invidious.io/instances.json"
                                          "?pretty=1&sort_by=type,users")
                          :then 'sync)))
              (thread-last
                (json-parse-string raw :object-type 'plist :array-type 'list)
                (cl-remove-if-not (lambda (s) (eq t (plist-get (cadr s) :api))))
                (mapcar #'car))))))

(defun cw--invidious-format (result)
  "Format Invidious search RESULT as a candidate string."
  (pcase (plist-get result :type)
    ("channel"
     (propertize
      (concat (propertize "[CHANNEL] "
               'face 'cw--invidious-published-face)
              " " (truncate-string-to-width (plist-get result :description) 60) " "
              (propertize " " 'display `(space :align-to ,(floor (* (window-width) 3) 5)))
              (propertize (if-let ((subs (plist-get result :subCount)))
                              (format "%4s subs" (file-size-human-readable subs 'si))
                            (make-string 11 ? ))
                          'face 'cw--invidious-length-face)
              " " (truncate-string-to-width
                   (propertize (plist-get result :author)
                               'face 'cw--invidious-author-face)
                   40 nil ? ))
      :url (format "https://www.youtube.com/%s/videos" (plist-get result :channelHandle))
      :author-url (format "https://www.youtube.com%s" (plist-get result :authorUrl))))
    ("playlist"
     (propertize
      (concat (propertize "[PLAYLIST]"
               'face 'cw--invidious-published-face)
              " " (plist-get result :title) " "
              (propertize " " 'display `(space :align-to ,(floor (* (window-width) 3) 5)))
              (propertize (format "%4d Videos " (plist-get result :videoCount))
                          'face 'cw--invidious-length-face)
              " " (truncate-string-to-width
                   (propertize (plist-get result :author)
                               'face 'cw--invidious-author-face)
                   40 nil ? ))
      :url (format "https://www.youtube.com/watch?list=%s"
                   (plist-get result :playlistId))
      :author-url (format "https://www.youtube.com%s"
                          (plist-get result :authorUrl))))
    ("video"
     (propertize
      (concat (propertize
               (format-time-string
                "%Y-%m-%d"
                (if-let ((published (plist-get result :published)))
                    (seconds-to-time published) 0))
               'face 'cw--invidious-published-face)
              " " (plist-get result :title) " "
              (propertize " " 'display `(space :align-to ,(- (floor (* (window-width) 3) 5)
                                                           12)))
              (and-let* ((duration (plist-get result :lengthSeconds))
                         (hours (floor duration 3600))
                         (minutes (floor (mod duration 3600) 60))
                         (seconds (mod duration 60)))
                (propertize
                 (format "(%02d:%02d:%02d) " hours minutes seconds)
                 'face 'cw--invidious-length-face))
              (propertize
               (if-let ((views (plist-get result :viewCount)))
                   (format "%4s views" (file-size-human-readable views 'si))
                 (make-string 11 ? ))
               'face 'cw--invidious-length-face)
              " " (truncate-string-to-width
                   (propertize (plist-get result :author)
                               'face 'cw--invidious-author-face)
                   40 nil ? ))
      :url (format "https://www.youtube.com/watch?v=%s"
                   (plist-get result :videoId))
      :author-url (format "https://www.youtube.com%s"
                          (plist-get result :authorUrl))))))

(defun cw--invidious-async ()
  "Return async builder for Invidious search.
A generation counter discards responses to stale queries, and
in-flight requests are deleted."
  (lambda (sink)
    (let ((generation 0) proc)
      (lambda (action)
        (pcase action
          ((pred stringp)
           (funcall sink 'flush)
           (setq generation (1+ generation))
           (when (processp proc)
             (delete-process proc))
           (when-let* ((api-url (car (cw--get-invidious-servers)))
                       (params (url-build-query-string
                                `(("q" ,action)
                                  ("page" "1")
                                  ("fields" "title,videoId,author,authorId,authorUrl,lengthSeconds,published")
                                  ("sort_by" "relevance")))))
             (let ((gen generation))
               (setq proc
                     (plz 'get (concat api-url "/api/v1/search?" params)
                       :as (lambda ()
                             (condition-case nil
                                 (json-parse-buffer :object-type 'plist)
                               (error nil)))
                       :then (lambda (response)
                               (when (and (eq gen generation) response)
                                 (funcall sink
                                          (seq-filter #'identity
                                                      (mapcar #'cw--invidious-format
                                                              response)))))
                       :else (lambda (err)
                               (message "cw: Invidious search failed: %S" err)))))))
          ('destroy
           (setq generation (1+ generation))
           (when (processp proc)
             (delete-process proc))
           (funcall sink action))
          (_ (funcall sink action)))))))

;;; Feature

(provide 'cw)
;;; cw.el ends here


;; Local Variables:
;; eval: (outline-minor-mode 1)
;; End:
