;;; bark.el --- Async web-to-org bookmarker  -*- lexical-binding: t; -*-

(require 'url)
(require 'dom)
(require 'org)
(require 'subr-x)


(defvar bark-bookmark-file
  (cond
   ;; MSYS2 / MinGW64 Emacs
   ((eq system-type 'ms-windows)
    (expand-file-name "Bookmarks.org" "/D/Documents/KB"))

   ;; Windows
   ((eq system-type 'windows-nt)
    (expand-file-name "Bookmarks.org" "D:/Documents/KB"))

   ;; macOS
   ((eq system-type 'darwin)
    (expand-file-name "~/Documents/Bookmarks.org"))

   ;; Linux / BSD / everything else
   (t
    (expand-file-name "Bookmarks.org" "/mnt/data/Documents/KB")))
  "Path to the Org file where bark stores bookmarks.")

;;(defvar bark-bookmark-file "~/Bookmarks.org"
;;  "Path to the Org file where bark stores bookmarks.")

(defun bark--extract-title (dom)
  "Extract the <title> text from DOM."
  (let ((title-node (dom-by-tag dom 'title)))
    (when title-node
      (string-trim (dom-text title-node)))))

(defun bark--extract-summary (dom)
  "Extract a simple summary from DOM."
  (or
   ;; meta description
   (let* ((meta (car (dom-by-tag dom 'meta)))
          (name (dom-attr meta 'name))
          (content (dom-attr meta 'content)))
     (when (and name (string-match-p "description" name))
       (string-trim content)))
   ;; first paragraph
   (let ((p (car (dom-by-tag dom 'p))))
     (when p
       (string-trim (dom-text p))))
   "No summary available."))

(defun bark--generate-tags (url title summary)
  "Generate a list of tags based on URL, title, and summary."
  (let* ((domain (url-host (url-generic-parse-url url)))
         (domain-tag (when domain (replace-regexp-in-string "\\." "_" domain)))
         (keywords (seq-take
                    (seq-filter
                     (lambda (w) (> (length w) 4))
                     (delete-dups
                      (split-string (concat title " " summary) "[^A-Za-z]+" t)))
                    5)))
    (delete-dups (append (list domain-tag) keywords))))

(defun bark--append-to-org (url title summary tags)
  "Append a new Org entry to `bark-bookmark-file`."
  (with-current-buffer (find-file-noselect bark-bookmark-file)
    (goto-char (point-max))
    (insert (format "* [[%s][%s]]\n" url title))
    (insert (format "  :PROPERTIES:\n  :ADDED: %s\n  :END:\n"
                    (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (when tags
      (insert (format "  :TAG: %s\n\n" (string-join tags " "))))
    (insert summary "\n\n")
    (save-buffer)))

(defun bark--process-response (status url)
  "Callback for async URL fetch."
  (if (plist-get status :error)
      (message "bark: Failed to retrieve %s" url)
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)
      (let* ((html (buffer-substring-no-properties (point) (point-max)))
             (dom (with-temp-buffer
                    (insert html)
                    (libxml-parse-html-region (point-min) (point-max))))
             (title (or (bark--extract-title dom) "Untitled Page"))
             (summary (bark--extract-summary dom))
             (tags (bark--generate-tags url title summary)))
        (bark--append-to-org url title summary tags)
        (message "bark: Added bookmark for %s" title))))
  (kill-buffer))

;;;###autoload
(defun bark (url)
  "Fetch URL asynchronously, extract title/summary, and store in ~/Bookmarks.org."
  (interactive "sEnter URL: ")
  (message "bark: Fetching %s..." url)
  (url-retrieve url #'bark--process-response (list url)))
