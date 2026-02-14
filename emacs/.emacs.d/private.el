;; set some variables used in configuration
(if (eq system-type 'gnu/linux)
    (progn
      (setq jeff/home "/home/jeff/")
      (setq jeff/dotfiles (concat jeff/home "dev/github.com/jemoore/dotfiles/"))
      (setq jeff/kb "/mnt/data/Documents/KB/")
      (setq jeff/md "/mnt/data/Documents/md/")))
(if (eq system-type 'windows-nt)
    (progn
      (if msystem
	  (setq jeff/home "C:/msys64/home/jeffe/")
	(setq jeff/home "C:/Users/jeffe/AppData/Roaming/"))
      (setq jeff/dotfiles (concat jeff/home "dev/repos/github/jemoore/dotfiles/"))
      (setq jeff/kb "e:/Documents/kb/")
      (setq jeff/md "e:/Documents/md/")))
      



(defun jm-extract-and-create-org-table ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "Number_of_things\\s-*=" nil t)
      (let ((start (line-end-position)))
        (when (re-search-forward "Number_of_Abilities\\s-*=" nil t)
          (let ((end (line-beginning-position)))
            (with-temp-buffer
              (insert "aaa");; (buffer-substring-no-properties start end))
              ;; (goto-char (point-min))
              ;; (while (re-search-forward "^\\s-*\\(Name\\|Number\\)\\s-*=\\s-*\\(.*\\)$" nil t)
              ;;   (replace-match "| \\1 | \\2 |"))
              ;; (org-mode)
              ;; (org-table-align)
              ;; (pop-to-buffer (current-buffer))))))))) 
	      )))))))


(defun jm-extract-and-create-org-table ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "Number_of_things\\s-*=" nil t)
      (let ((start (line-end-position)))
        (when (re-search-forward "Number_of_Abilities\\s-*=" nil t)
          (let ((end (line-beginning-position))
                (temp-buffer (generate-new-buffer "*temp-buffer*"))
		(oldbuf (current-buffer)))
            (with-current-buffer temp-buffer
              (insert-buffer-substring oldbuf start end))
              (goto-char (point-min))
              (while (re-search-forward "^\\s-*\\(Name\\|Number\\)\\s-*=\\s-*\\(.*\\)$" nil t)
                (replace-match "| \\1 | \\2 |"))
              (org-mode)
              (org-table-align)))))))

