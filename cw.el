;;; cw.el --- multi-source async search with Consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.1") (plz "0.7"))
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

;; 

;;; Code:
(require 'consult)
(require 'plz)

(defun cw--multi-async (async sources)
  "Merge the results of (a)sync SOURCES and pass it to function ASYNC."
  (let ((candidates (make-vector (length sources) nil)))
    (lambda (action)
      (pcase action
        ((pred stringp)
         (unless (equal action "")
           (let ((idx 0))
             (seq-doseq (src sources)
               (let* ((face (and (plist-member src :face) `(face ,(plist-get src :face))))
                     (cat (plist-get src :category))
                     (items (plist-get src :items))
                     (narrow (plist-get src :narrow))
                     (type (or (car-safe narrow) narrow -1))
                     (pos idx))
                 (when (or (eq consult--narrow type)
                           (not (or consult--narrow (plist-get src :hidden))))
                   (condition-case nil
                       (progn
                         (when (functionp items) (setq items (funcall items action)))
                         (aset candidates idx    ; sync source, refresh now
                               (and items (cw--multi-propertize
                                           items cat idx face)))
                         (funcall async 'flush)
                         (funcall async (apply #'append (append candidates nil))))
                     (wrong-number-of-arguments
                      (funcall items action      ; async source, refresh in callback
                               (lambda (response-items)
                                 (when response-items
                                   (aset candidates pos
                                         (cw--multi-propertize response-items cat pos face))
                                   (funcall async 'flush)
                                   (funcall async (apply #'append (append candidates nil))))))))))
               (cl-incf idx)))))
        (_ (funcall async action))))))

(defun cw--multi-propertize (response-items category pos &optional face)
  "Propertize RESPONSE-ITEMS with the multi-category datum and FACE.

POS and CATEGORY are the group ID and category for these items."
  (let ((annotated-items))
    (dolist (item response-items annotated-items)
      (let ((cand (consult--tofu-append item pos)))
        ;; Preserve existing `multi-category' datum of the candidate.
        (if (get-text-property 0 'multi-category cand)
            (when face (add-text-properties 0 (length item) face cand))
          ;; Attach `multi-category' datum and face.
          (add-text-properties 0 (length item)
                               `(multi-category (,category . ,item) ,@face) cand))
        (push cand annotated-items)))))

(defun cw--annotate (sources cand)
  (let ((src (consult--multi-source sources cand)))
    (if-let ((fun (plist-get src :annotate)))
        (funcall fun (cdr (get-text-property 0 'multi-category cand)))
      (plist-get src :name))))

(defun cw--multi (sources &rest options)
  (let* ((sources (consult--multi-enabled-sources sources))
         (selected
          (apply #'consult--read
                 (consult--async-split
                  (consult--async-throttle
                   (cw--multi-async
                    (consult--async-refresh-timer
                     (consult--async-sink))
                    sources)))
                 (append
                  options
                  (list
                   :sort        nil
                   :history     'cw--search-history
                   :initial     (consult--async-split-initial nil)
                   :category    'multi-category
                   :predicate   (apply-partially #'consult--multi-predicate sources)
                   :annotate    (apply-partially #'cw--annotate sources)
                   :group       (apply-partially #'consult--multi-group sources)
                   :lookup      (apply-partially #'consult--multi-lookup sources)
                   :preview-key (consult--multi-preview-key sources)
                   :narrow      (consult--multi-narrow sources)
                   :state       (consult--multi-state sources))))))
    (if (plist-member (cdr selected) :match)
        (when-let (fun (plist-get (cdr selected) :new))
          (funcall fun (car selected))
          (plist-put (cdr selected) :match 'new))
      (when-let (fun (plist-get (cdr selected) :action))
        (funcall fun (car selected)))
      (setq selected `(,(car selected) :match t ,@(cdr selected))))
    selected))


;;; Commands
(defun cw-search ()
  (interactive)
  (let ((consult-async-input-throttle 0.7)
        (consult-async-input-debounce 0.4))
    (cw--multi (list cw-source-elfeed cw-source-gptel
                     cw-source-brave cw-source-wombag
                     cw-source-browser-hist
                     cw-source-invidious)
               :prompt "Search: "
               :preview-key "M-RET")))

(defun cw-search-local ()
  (interactive)
  (let ((consult-async-input-throttle 0.7)
        (consult-async-input-debounce 0.4))
    (cw--multi (list cw-source-elfeed
                     cw-source-wombag
                     cw-source-browser-hist)
               :prompt "Search (local sources): "
               :preview-key "M-RET")))

;;; Convenience
(defvar cw--search-history nil
  "History variable for `cw-search' and co.")

(defvar cw--count 5
  "Max number of results per source.")

;;; Sources
;;;; gptel
(defvar cw-source-gptel
  `(:name     "gptel"
    :narrow   ?g
    :category 'consult-web
    :face     font-lock-operator-face
    :annotate ,#'cw-gptel-annotate
    :state    ,#'cw--gptel-state
    :items    ,#'cw--gptel-request
    :enabled  ,(lambda () (fboundp 'gptel))))

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

(defun cw--gptel-request (query callback)
  ""
  (let ((gptel-max-tokens 24)
        (gptel-use-curl))
    (gptel-request query
      :system "Respond in 10 words or less."
      :callback
      (lambda (response _)
        (when response
          (setq response
                (propertize (string-trim-right response)
                            :title response
                            :source "gptel"
                            :query query
                            :model gptel-model
                            :stream gptel-stream
                            :backend (gptel-backend-name gptel-backend)))
          (funcall callback (list response)))))))

(defun cw-gptel-annotate (cand)
  ""
  (let* ((props (text-properties-at 0 cand))
         (model (plist-get props :model))
         (stream (plist-get props :stream))
         (backend (plist-get props :backend)))
    (concat " " (propertize " " 'display '(space :align-to center))
            (propertize backend 'face 'font-lock-variable-name-face)
            (propertize (format ":%s" model) 'face 'font-lock-warning-face)
            (and stream (propertize " ~stream~ " 'face 'font-lock-comment-face)))))

;;;; brave
(defvar cw-source-brave
  `(:name     "Brave"
    :narrow   ?b
    :category consult-web
    :state    ,#'cw--brave-state
    :items    ,#'cw--brave-request
    :enabled  ,(lambda () cw-brave-api-key)))

(defun cw--eww-readable-once ()
  (eww-readable)
  (remove-hook 'eww-after-render-hook #'cw--eww-readable-once))

(defun cw--brave-state ()
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

(defvar url-http-end-of-headers)

(defun cw--url-parse-buffer ()
  (goto-char url-http-end-of-headers)
  (condition-case nil
      (json-parse-buffer :object-type 'plist)
    (error nil)))

(defun cw--brave-request (query callback)
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("User-Agent" . "Emacs:consult-web/0.1 (Emacs consult-web package; https://github.com/armindarvish/consult-web)")
           ("Accept" . "application/json")
           ("Accept-Encoding" . "gzip")
           ("X-Subscription-Token" . ,(let ((key cw-brave-api-key))
                                       (if (functionp key) (funcall key) key))))))
    (url-retrieve (cw-brave-url-string query)
                  (lambda (_)
                    (when-let* ((attrs (cw--url-parse-buffer))
                                (raw-results (map-nested-elt attrs '(:web :results)))
                                (annotated-results
                                 (mapcar
                                  (lambda (item)
                                    (let* ((title (map-elt item :title))
                                           (search-url (cw-brave-url-string query))
                                           (url (map-elt item :url))
                                           (urlobj (and url (url-generic-parse-url url)))
                                           (domain (and (url-p urlobj) (url-domain urlobj))) ;we modify domain here
                                           (domain (and (stringp domain)
                                                        (propertize domain 'face 'font-lock-variable-name-face)))
                                           (path (and (url-p urlobj) (url-filename urlobj)))
                                           (path (and (stringp path)
                                                      (propertize path 'face 'font-lock-warning-face)))
                                           (decorated (concat title "\t"
                                                              (propertize " " 'display '(space :align-to center))
                                                              domain path
                                                              )))
                                      (propertize decorated
                                                  :title title
                                                  :url url
                                                  :search-url search-url
                                                  :query query)))
                                  raw-results)))
                      (funcall callback annotated-results)))
                  nil 'silent)))

(defvar cw-brave-url "https://api.search.brave.com/res/v1/web/search")
(defvar cw-brave-api-key nil)

(defun cw-brave-url-string (query)
  (concat cw-brave-url "?"
          (url-build-query-string
           `(("q" ,(url-hexify-string query))
             ("count" ,(format "%s" cw--count))
             ("page" ,(format "%s" 0))))))

;;;; Elfeed
(defvar cw-source-elfeed
  `(:name     "Elfeed"
    :narrow   ?e
    :category consult-web
    :items    ,#'cw--elfeed-search
    :state    ,#'cw--elfeed-state
    :enabled  ,(lambda () (boundp 'elfeed-db))))

(defun cw--elfeed-state ()
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
  (let* ((elfeed-search-filter (concat (format "#%d " cw--count) query))
         (filter (elfeed-search-parse-filter elfeed-search-filter))
         (head (list nil)) (tail head) (count 0)
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
             (title-width ;; (- (window-width) 20 elfeed-search-trailing-width)
              60
                          )
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

;;;; Wombag
(defvar cw-source-wombag
  `(:name     "Wallabag"
    :narrow   ?w
    :category consult-web
    :items    ,#'cw--wombag-search
    :state    ,#'cw--wombag-state
    :enabled  ,(lambda () (featurep 'wombag-search))))

(defun cw--wombag-search (query)
  (let* ((wombag-search-filter (concat (format "#%d " cw--count) query))
         (filter (wombag-search-parse-filter
                  wombag-search-filter wombag-search-columns))
         (entries (wombag-db-get-entries filter wombag-search-columns)))
    (when entries
      (let ((wombag-search-title-width 60))
        (mapcar (lambda (entry) (propertize (wombag-search-format-entry entry)
                                       :entry entry
                                       :url (alist-get 'url entry)))
                entries)))))

(defun cw--wombag-state ()
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

;;;; Browser hist
(defvar cw-source-browser-hist
  `(:name     "Browser history"
    :narrow   ?h
    :category consult-web
    :items    ,#'cw--browser-hist-search
    :state    ,#'cw--brave-state
    :enabled  ,(lambda () (fboundp 'browser-hist-search))))

(defun cw--browser-hist-search (query)
  (when (require 'browser-hist nil t)
    (when-let ((results (browser-hist--send-query query)))
      (mapcar (pcase-lambda (`(,url . ,title))
                (let* ((urlobj (and url (url-generic-parse-url url)))
                       (domain (AND (URL-P urlobj) (url-domain urlobj)))
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


;;;; Invidious
(defvar cw-source-invidious
  `(:name     "Youtube"
    :narrow   ?y
    :category consult-web
    :items    ,#'cw--invidious-search
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

(defvar cw--invidious-servers nil)

(defun cw--invidious-state ()
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

(defun cw--invidious-search (terms callback)
  (let* ((params (url-build-query-string
                  `(("q" ,terms)
                    ("page" "1")
                    ;; Fields are ignored right now, this is for the future
                    ("fields" "title,videoId,author,authorId,authorUrl,lengthSeconds,published")
                    ;; ("type" "video" "playlist")
                    ("sort_by" "relevance"))))
         (api-url (car (cw--get-invidious-servers)))
         (query-url (concat api-url "/api/v1/search?" params)))
    (plz 'get query-url
      :as (lambda () (json-parse-buffer :object-type 'plist))
      :then
      (lambda (response)
        "Return list of candidates to cw."
        (when response
          (thread-last response
            ;; (seq-take response cw--count)
            (mapcar
             (lambda (result)
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
                                       (plist-get result :authorUrl)))))))
            (seq-filter (lambda (result) (not (null result))))
            (funcall callback)))))))

;;; Feature

(provide 'cw)
;;; cw.el ends here


;; Local Variables:
;; eval: (outline-minor-mode 1)
;; End:
