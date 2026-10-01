;;; private.el --- Machine-local path overrides -*- lexical-binding: t; -*-

;; Loaded by init.el immediately after its default paths are defined and
;; before anything is derived from them.  init.el uses `defvar', which does
;; not clobber an already-bound variable, so a plain `setq' here wins.
;;
;; The macOS defaults in init.el are already correct (~/dev/github.com/jemoore/...),
;; so this file only needs to override the machines whose layout differs.
;;
;; Keep this file to path settings only.  Anything else belongs in init.el or
;; in custom-emacs/.

;;; Code:

(pcase system-type
  ;; macOS: init.el's defaults already match this machine.  Nothing to do.
  ('darwin nil)

  ('gnu/linux
   (setq jem/home "/home/jeff/"
         jem/dev "/home/jeff/dev/github.com/jemoore/"
         jem/dotfiles "/home/jeff/dev/github.com/jemoore/stow-dotfiles/"
         jem/kb "/mnt/data/Documents/KB/"
         jem/md "/mnt/data/Documents/md/"))

  ('windows-nt
   ;; MSYSTEM_PREFIX is set only under MSYS2/MinGW.
   (let* ((msys (let ((v (getenv "MSYSTEM_PREFIX")))
                  (and v (not (string-empty-p v)))))
          (home (if msys "C:/msys64/home/jeffe/" "C:/Users/jeffe/AppData/Roaming/")))
     (setq jem/home home
           jem/dotfiles (concat home "dev/github.com/jemoore/stow-dotfiles/")
           jem/kb "e:/Documents/kb/"
           jem/md "e:/Documents/md/")
     (when msys
       (setq package-gnupghome-dir "/home/jeff/.emacs.d/elpa/gnupg")))))

(provide 'private)
;;; private.el ends here
