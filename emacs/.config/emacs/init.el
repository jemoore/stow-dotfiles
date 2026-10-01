;;; init.el --- Jeff Moore's Emacs configuration -*- lexical-binding: t; -*-

;; Emacs 31, evil keybindings, Eglot + tree-sitter for code, Org for notes and
;; agenda, Denote for the org knowledge base, markdown-mode for the md notes.
;;
;; Layout:
;;   early-init.el  startup/GC/frame tuning (loaded first by Emacs)
;;   init.el        this file
;;   private.el     optional, machine-local path overrides (see "Paths")
;;   custom.el      written by Customize; loaded last so it wins
;;
;; Conventions: every function and variable defined here is prefixed `jem/'.
;; Sections are delimited by `;;;' headings -- `C-c o' (consult-outline) or
;; `M-x imenu' will navigate them.

;;; Code:

;;;; ---------------------------------------------------------------- Startup --

;; Undo the early-init.el startup hacks once loading is done, and report how
;; long it took.  A steady-state `gc-cons-threshold' of 64MB is high enough to
;; avoid GC pauses while typing without letting the heap grow unbounded.
(defun jem/startup-restore ()
  "Restore GC and file-handler settings deferred by `early-init.el'."
  (setq gc-cons-threshold (* 64 1024 1024)
        gc-cons-percentage 0.2
        file-name-handler-alist jem/file-name-handler-alist)
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2fs"
                   (float-time (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'jem/startup-restore)

;; Collect when idle rather than mid-keystroke.
(run-with-idle-timer 5 t #'garbage-collect)

;;;; ------------------------------------------------------------------ Paths --

;; Defaults are derived from $HOME so a fresh machine works with no extra
;; setup.  private.el (if present) can override any of these -- it is the only
;; machine-local file, and it should contain nothing but `setq' forms.

(defvar jem/home (expand-file-name "~/")
  "Home directory, with trailing slash.")

(defvar jem/dev (expand-file-name "dev/github.com/jemoore/" jem/home)
  "Root of personal repository checkouts.")

(defvar jem/dotfiles (expand-file-name "stow-dotfiles/" jem/dev)
  "Path to the stow-dotfiles checkout.")

(defvar jem/kb (expand-file-name "KB/" jem/dev)
  "Org knowledge base.  Also `denote-directory'.")

(defvar jem/md (expand-file-name "mdnotes/" jem/dev)
  "Markdown notes directory.")

(defvar jem/lisp-dir (expand-file-name "custom-emacs/" user-emacs-directory)
  "Hand-written elisp that is not a package.")

;; Machine-local overrides, loaded before anything is derived from the paths
;; above.  `defvar' does not clobber an already-bound variable, so private.el
;; may set any of these with plain `setq' and win.
(let ((private (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private)
    (load private nil 'nomessage)))

;; Derived AFTER private.el, so overriding `jem/kb' also moves the agenda.
(defvar jem/agenda-dir (expand-file-name "agenda/" jem/kb)
  "Directory holding the small set of files that feed `org-agenda'.")

(when (file-directory-p jem/lisp-dir)
  (add-to-list 'load-path (directory-file-name jem/lisp-dir)))

;; Keep generated state out of the tangle/stow target and out of git.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      bookmark-default-file (expand-file-name "bookmarks" jem/lisp-dir)
      custom-theme-directory (expand-file-name "themes" jem/lisp-dir))

;;;; --------------------------------------------------------- Package system --

(require 'package)

;; NonGNU ELPA is needed for a handful of packages (e.g. `popon') that are not
;; on GNU ELPA or MELPA.  The old orgmode.org archive is deliberately absent:
;; it is retired and frozen at Org 9.5, which would silently downgrade the
;; Org 9.8 that ships with Emacs 31.
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/"))
      ;; Prefer stable GNU ELPA versions when a package exists in both places.
      package-archive-priorities
      '(("gnu" . 10) ("nongnu" . 5) ("melpa" . 0)))

(package-initialize)

(defvar jem/package-refresh-max-age (* 7 24 60 60)
  "Refresh the package index when the cached copy is older than this (seconds).")

(defun jem/package-index-age ()
  "Age in seconds of the cached MELPA index, or nil if there is none."
  (let ((index (expand-file-name "archives/melpa/archive-contents" package-user-dir)))
    (when (file-exists-p index)
      (float-time
       (time-subtract (current-time)
                      (file-attribute-modification-time (file-attributes index)))))))

;; Refresh when the index is missing OR stale.  Checking only for an *empty*
;; `package-archive-contents' is not enough: MELPA rebuilds packages
;; continuously and deletes superseded tarballs, so a cached index more than a
;; few days old routinely points at files that now 404.  `package-install'
;; then fails, and because a failed install can abort the rest of this file,
;; every `use-package' form below it is silently skipped.
(let ((age (jem/package-index-age)))
  (when (or (null package-archive-contents) (null age)
            (> age jem/package-refresh-max-age))
    (message "Package index is %s; refreshing..."
             (if age (format "%.0f days old" (/ age 86400.0)) "missing"))
    (package-refresh-contents)))

;; use-package is built into Emacs 29+; no bootstrap needed.
(require 'use-package)
(setq use-package-always-ensure t         ; install packages that aren't present
      ;; Deliberately NOT `use-package-expand-minimally': that strips the
      ;; error-handling wrappers around each :init/:config body, so one broken
      ;; or unavailable package aborts every form after it in this file.  The
      ;; extra generated code is irrelevant; isolating failures is not.
      use-package-expand-minimally nil
      use-package-compute-statistics nil) ; flip to t, then M-x use-package-report

;;;; --------------------------------------------------------- Core behaviour --

(use-package emacs
  :ensure nil
  :custom
  ;; Answering prompts
  (use-short-answers t)          ; y/n instead of yes/no
  (confirm-kill-emacs #'y-or-n-p)
  ;; Never use native dialogs.  On macOS `x-popup-dialog' opens a modal Cocoa
  ;; window that blocks Emacs' event loop; if it lands behind the frame or on
  ;; another Space, Emacs appears frozen and cannot be quit.  Keeping every
  ;; prompt in the minibuffer means C-g always works.
  (use-dialog-box nil)
  (use-file-dialog nil)

  ;; Editing
  (indent-tabs-mode nil)
  (tab-width 4)
  (standard-indent 4)
  (require-final-newline t)
  (sentence-end-double-space nil)
  (save-interprogram-paste-before-kill t)
  (mouse-yank-at-point t)
  (kill-do-not-save-duplicates t)

  ;; Files
  (delete-by-moving-to-trash t)
  (create-lockfiles nil)
  (make-backup-files t)
  (version-control t)
  (backup-by-copying t)         ; don't clobber symlinks (stow!)
  (delete-old-versions t)
  (kept-old-versions 2)
  (kept-new-versions 6)
  (auto-save-default t)
  (auto-save-no-message t)

  ;; Display
  (visible-bell t)
  (ring-bell-function #'ignore)
  (frame-resize-pixelwise t)
  (window-resize-pixelwise t)
  (cursor-in-non-selected-windows nil)
  (highlight-nonselected-windows nil)
  (x-stretch-cursor t)
  (blink-cursor-interval 0.6)
  (fast-but-imprecise-scrolling t)
  (inhibit-compacting-font-caches t)
  (redisplay-skip-fontification-on-input t)

  ;; Scrolling: keep a margin, never jump the cursor to the middle.
  (scroll-margin 3)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)
  (mouse-wheel-follow-mouse t)
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-scroll-amount '(2 ((shift) . 4) ((control) . 6)))

  ;; Minibuffer
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (resize-mini-windows 'grow-only)
  (echo-keystrokes 0.02)

  ;; Bidirectional text: disabling it is a real redisplay win and I only ever
  ;; write left-to-right.
  (bidi-inhibit-bpa t)

  :config
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  (setq backup-directory-alist
        `(("." . ,(expand-file-name "backup/" user-emacs-directory)))
        auto-save-file-name-transforms
        `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))

  ;; macOS has a native trash; only Linux needs an explicit directory.
  (when (eq system-type 'gnu/linux)
    (setq trash-directory (expand-file-name ".local/share/Trash/files" jem/home)))

  ;; Encoding
  (set-charset-priority 'unicode)
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)

  ;; Firefox has a built-in browse-url handler; Brave does not (there is no
  ;; `browse-url-brave' or `browse-url-brave-program' in Emacs -- the previous
  ;; config set both, so its Brave binding never worked).  See
  ;; `jem/browse-url-brave' under "Helper commands".
  (setq browse-url-firefox-program
        (if (eq system-type 'darwin)
            "/Applications/Firefox.app/Contents/MacOS/firefox"
          "/usr/bin/firefox"))

  ;; Useful global modes.
  (delete-selection-mode 1)          ; typing replaces the region
  (global-auto-revert-mode 1)        ; pick up external changes
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (winner-mode 1)                    ; undo window layout changes
  (repeat-mode 1)                    ; C-x o o o ... instead of C-x o C-x o
  (context-menu-mode 1)              ; right-click menu
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))

  ;; Treat underscore as a word constituent in code.
  (defun jem/underscore-is-word ()
    "Make `_' part of a word in the current buffer."
    (modify-syntax-entry ?_ "w"))
  (add-hook 'prog-mode-hook #'jem/underscore-is-word))

;; GUI Emacs launched from Finder/Dock does not inherit the shell PATH, so
;; clangd, gopls, rust-analyzer, sbcl, rg and fd are all invisible to it.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;;;; ----------------------------------------------------------- Session state --

(use-package recentf
  :ensure nil
  :init (recentf-mode 1)
  :custom
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'never)
  (recentf-exclude `(,(expand-file-name "elpa/" user-emacs-directory)
                     ,(expand-file-name "backup/" user-emacs-directory)
                     "/tmp/" "/ssh:" "\\.gz\\'")))

(use-package savehist
  :ensure nil
  :init (savehist-mode 1)
  :custom
  (history-length 300)
  (savehist-additional-variables '(search-ring regexp-search-ring kill-ring)))

(use-package saveplace
  :ensure nil
  :init (save-place-mode 1)
  :custom (save-place-limit 200))

(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))

;; Save on idle and when switching windows.  With `super-save' on, an explicit
;; :w is rarely needed, and auto-revert keeps other buffers in sync.
(use-package super-save
  :defer 2
  :diminish super-save-mode
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 5)
  (super-save-exclude '(".gpg"))
  :config
  (add-to-list 'super-save-triggers 'evil-window-next)
  (add-to-list 'super-save-triggers 'evil-window-prev)
  (super-save-mode 1))

;;;; ---------------------------------------------------------------- Fonts --

(defvar jem/fixed-font "ShureTechMono Nerd Font"
  "Monospace font.  Falls back to the first available in `jem/font-fallbacks'.")

(defvar jem/variable-font "GoMono Nerd Font"
  "Font for prose (Org, Markdown, Info).")

(defvar jem/font-size 160
  "Default font height, in 1/10 pt.  160 = 16pt.")

(defvar jem/font-fallbacks
  '("ShureTechMono Nerd Font" "GoMono Nerd Font" "JetBrains Mono"
    "Menlo" "DejaVu Sans Mono" "Monospace")
  "Ordered candidates used when `jem/fixed-font' is not installed.")

(defun jem/first-available-font (candidates)
  "Return the first font in CANDIDATES that exists, or nil."
  (seq-find (lambda (f) (find-font (font-spec :family f))) candidates))

(defun jem/set-fonts (&optional frame)
  "Apply font faces to FRAME.  No-op on terminal frames, which use the host font."
  (when (display-graphic-p frame)
    (let ((fixed (or (jem/first-available-font (cons jem/fixed-font jem/font-fallbacks))
                     "Monospace"))
          (variable (or (jem/first-available-font (cons jem/variable-font jem/font-fallbacks))
                        "Sans Serif")))
      (set-face-attribute 'default frame :family fixed :height jem/font-size)
      (set-face-attribute 'fixed-pitch frame :family fixed :height 1.0)
      (set-face-attribute 'variable-pitch frame :family variable :height 1.0))))

;; Under a daemon, faces must be set per-frame as clients connect; the
;; `display-graphic-p' guard inside `jem/set-fonts' handles terminal clients.
(if (daemonp)
    (add-hook 'after-make-frame-functions #'jem/set-fonts)
  (jem/set-fonts))

;;;; ------------------------------------------------------------------- UI --

(use-package ef-themes
  :config
  ;; NO-CONFIRM (the trailing t) matters: without it `load-theme' prompts, and
  ;; under a daemon or `emacs -nw' there is nobody to answer, blocking startup.
  (load-theme 'ef-dream t))

(use-package nerd-icons
  ;; One-time per machine: M-x nerd-icons-install-fonts, then restart Emacs
  ;; (kill the daemon too).  Installs "Symbols Nerd Font Mono".
  :custom (nerd-icons-scale-factor 1.0))

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-minor-modes nil))

;; which-key is built into Emacs 30+.
(use-package which-key
  :ensure nil
  :init (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.6)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 36))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key)
  ([remap describe-symbol]   . helpful-symbol))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Line numbers in code and config, never in prose, terminals or images.
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode conf-mode) . display-line-numbers-mode)
  :custom (display-line-numbers-width 3))

(column-number-mode 1)
(global-visual-line-mode -1)            ; opt in per-mode instead
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(show-paren-mode 1)
(setq show-paren-delay 0
      show-paren-context-when-offscreen 'overlay)

;;;; ----------------------------------------------------------------- Evil --

(use-package evil
  :init
  ;; These must be set before evil loads.
  (setq evil-want-integration t
        evil-want-keybinding nil        ; evil-collection owns the rest
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-want-Y-yank-to-eol t
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-redo     ; built-in since Emacs 28
        evil-search-module 'evil-search
        evil-split-window-below t
        evil-vsplit-window-right t)
  :config
  (evil-mode 1)

  (define-key evil-insert-state-map (kbd "C-g") #'evil-normal-state)

  ;; Move by visual line, so wrapped prose behaves the way it looks.
  (evil-global-set-key 'motion "j" #'evil-next-visual-line)
  (evil-global-set-key 'motion "k" #'evil-previous-visual-line)

  ;; ESC quits prompts everywhere.
  (global-set-key (kbd "<escape>") #'keyboard-escape-quit)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'vterm-mode 'insert))

;; `jj' leaves insert state.  key-chord is a tiny package and the well-tested
;; way to do this; hand-rolled `read-event' versions break keyboard macros.
(use-package key-chord
  :after evil
  :custom (key-chord-two-keys-delay 0.25)
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jj" #'evil-normal-state))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer nil)
  :config (evil-collection-init))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;;;; ------------------------------------------------------- Helper commands --

;; The previous config bound several Spacemacs command names that were never
;; actually defined (find-user-init-file, load-user-init-file,
;; rename-file-and-buffer).  These are the real implementations.

(defun jem/find-init-file ()
  "Open this init.el."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun jem/reload-init-file ()
  "Re-evaluate init.el.  Note that this adds to state; it does not reset it."
  (interactive)
  (load (expand-file-name "init.el" user-emacs-directory) nil 'nomessage)
  (message "init.el reloaded"))

(defun jem/rename-file-and-buffer (new-name)
  "Rename the current file and its buffer to NEW-NAME."
  (interactive
   (list (read-file-name "Rename to: " nil nil nil
                         (file-name-nondirectory (or (buffer-file-name) "")))))
  (let ((file (buffer-file-name)))
    (unless (and file (file-exists-p file))
      (user-error "Buffer is not visiting a file"))
    (rename-file file new-name 1)
    (set-visited-file-name new-name t t)
    (message "Renamed to %s" new-name)))

(defun jem/delete-file-and-buffer ()
  "Delete the current file (to trash) and kill its buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file (user-error "Buffer is not visiting a file"))
    (when (yes-or-no-p (format "Delete %s? " file))
      (delete-file file 'trash)
      (kill-buffer))))

(defun jem/find-file-in-dotfiles ()
  "Find a file inside the stow-dotfiles checkout."
  (interactive)
  (let ((default-directory jem/dotfiles))
    (call-interactively #'find-file)))

(defun jem/copy-buffer-file-path ()
  "Put the current buffer's file path on the kill ring."
  (interactive)
  (let ((path (or (buffer-file-name) default-directory)))
    (kill-new path)
    (message "%s" path)))

(defvar jem/brave-program
  (pcase system-type
    ('darwin "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser")
    (_ (or (executable-find "brave-browser") (executable-find "brave"))))
  "Path to the Brave binary, or nil if not installed.")

(defun jem/browse-url-brave (url &optional _new-window)
  "Open URL in Brave.  Emacs has no built-in handler for this browser."
  (interactive (browse-url-interactive-arg "URL: "))
  (unless (and jem/brave-program (file-executable-p jem/brave-program))
    (user-error "Brave not found%s"
                (if jem/brave-program (format " at %s" jem/brave-program) "")))
  (start-process "brave" nil jem/brave-program url))

(defun jem/sudo-find-file (file)
  "Open FILE as root over TRAMP."
  (interactive "FOpen as root: ")
  (find-file (concat "/sudo:root@localhost:" (expand-file-name file))))

;;;; ---------------------------------------------------------- Leader keys --

;; SPC is the leader in normal/visual/motion state; C-SPC works everywhere,
;; including insert state and the minibuffer.  `:keymaps 'override' makes the
;; leader win against major-mode maps.

(use-package general
  :after evil
  :demand t
  :config
  (general-evil-setup)
  (general-create-definer jem/leader
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; A `,' local leader for mode-specific commands (org, elisp, ...).
  (general-create-definer jem/local-leader
    :states '(normal visual motion)
    :prefix ","))

(jem/leader
  "SPC" '(execute-extended-command :which-key "M-x")
  "TAB" '(mode-line-other-buffer   :which-key "last buffer")
  ":"   '(eval-expression          :which-key "eval")
  "/"   '(consult-ripgrep          :which-key "search project")
  "x"   (general-simulate-key "C-x")
  "u"   (general-simulate-key "C-u")

  ;; Applications
  "a"  '(:ignore t :which-key "apps")
  "ac" '(calc                    :which-key "calc")
  "ae" '(elfeed                  :which-key "elfeed")
  "ag" '(gptel                   :which-key "gptel chat")
  "aG" '(gptel-send              :which-key "gptel send")
  "as" '(sly                     :which-key "sly repl")
  "at" '(vterm                   :which-key "vterm")
  "aT" '(eshell                  :which-key "eshell")
  "ab" '(:ignore t               :which-key "browse")
  "abf" '(browse-url-firefox     :which-key "firefox")
  "abb" '(jem/browse-url-brave   :which-key "brave")
  "abe" '(eww                    :which-key "eww")

  ;; Buffers
  "b"  '(:ignore t :which-key "buffer")
  "bb" '(consult-buffer          :which-key "switch buffer")
  "bd" '(kill-current-buffer     :which-key "kill buffer")
  "bD" '(jem/delete-file-and-buffer :which-key "delete file+buffer")
  "bi" '(consult-imenu           :which-key "imenu")
  "bk" '(kill-buffer             :which-key "kill (choose)")
  "bn" '(next-buffer             :which-key "next")
  "bp" '(previous-buffer         :which-key "previous")
  "br" '(revert-buffer-quick     :which-key "revert")
  "bR" '(jem/rename-file-and-buffer :which-key "rename file")
  "bs" '(scratch-buffer          :which-key "scratch")

  ;; Code
  "c"  '(:ignore t :which-key "code")
  "ca" '(eglot-code-actions      :which-key "code action")
  "cc" '(compile                 :which-key "compile")
  "cC" '(recompile               :which-key "recompile")
  "cd" '(xref-find-definitions   :which-key "definition")
  "cD" '(eglot-find-declaration  :which-key "declaration")
  "cf" '(eglot-format            :which-key "format")
  "ci" '(eglot-find-implementation :which-key "implementation")
  "cr" '(eglot-rename            :which-key "rename symbol")
  "cR" '(xref-find-references    :which-key "references")
  "cs" '(consult-eglot-symbols   :which-key "workspace symbols")
  "cx" '(consult-flymake         :which-key "diagnostics")
  "cl" '(eglot                   :which-key "start eglot")
  "cL" '(eglot-shutdown          :which-key "stop eglot")

  ;; Files
  "f"  '(:ignore t :which-key "files")
  "ff" '(find-file               :which-key "find file")
  "fd" '(jem/find-file-in-dotfiles :which-key "dotfiles")
  "fi" '(jem/find-init-file      :which-key "init.el")
  "fI" '(jem/reload-init-file    :which-key "reload init.el")
  "fl" '(find-file-literally     :which-key "find literally")
  "fr" '(consult-recent-file     :which-key "recent")
  "fs" '(save-buffer             :which-key "save")
  "fS" '(write-file              :which-key "save as")
  "fy" '(jem/copy-buffer-file-path :which-key "copy path")
  "fu" '(jem/sudo-find-file      :which-key "find as root")

  ;; Git
  "g"  '(:ignore t :which-key "git")
  "gg" '(magit-status            :which-key "status")
  "gb" '(magit-blame             :which-key "blame")
  "gl" '(magit-log-buffer-file   :which-key "file log")
  "gL" '(magit-log-all           :which-key "repo log")
  "gd" '(magit-diff-buffer-file  :which-key "file diff")
  "gc" '(magit-clone             :which-key "clone")
  "gf" '(magit-file-dispatch     :which-key "file dispatch")

  ;; Help
  "h"  (general-simulate-key "C-h" :which-key "help")

  ;; Notes: Org KB (Denote) and markdown
  "n"  '(:ignore t :which-key "notes")
  "nn" '(denote                  :which-key "new org note")
  "nN" '(denote-type             :which-key "new note (type)")
  "nf" '(consult-denote-find     :which-key "find org note")
  "ng" '(consult-denote-grep     :which-key "grep org notes")
  "nl" '(denote-link             :which-key "insert link")
  "nb" '(denote-backlinks        :which-key "backlinks")
  "nr" '(denote-rename-file      :which-key "rename note")
  "nd" '(denote-dired            :which-key "dired KB")
  "nk" '(jem/kb-dired            :which-key "open KB")
  "nm" '(:ignore t               :which-key "markdown")
  "nmf" '(jem/md-find-file       :which-key "find md note")
  "nmg" '(jem/md-grep            :which-key "grep md notes")
  "nmm" '(jem/md-open-moc        :which-key "MOC.md")
  "nmd" '(jem/md-dired           :which-key "dired mdnotes")

  ;; Org
  "o"  '(:ignore t :which-key "org")
  "oa" '(org-agenda              :which-key "agenda")
  "oc" '(org-capture             :which-key "capture")
  "oi" '(jem/org-open-inbox      :which-key "inbox.org")
  "ot" '(jem/org-open-todo       :which-key "todo.org")
  "op" '(jem/org-open-projects   :which-key "projects.org")
  "oj" '(jem/org-open-journal    :which-key "journal.org")
  "ol" '(org-store-link          :which-key "store link")
  "oq" '(jem/org-agenda-dashboard :which-key "dashboard")
  "oC" '(org-clock-goto          :which-key "goto clock")

  ;; Projects (built-in project.el)
  "p"  '(:ignore t :which-key "project")
  "pp" '(project-switch-project  :which-key "switch project")
  "pf" '(project-find-file       :which-key "find file")
  "pb" '(consult-project-buffer  :which-key "project buffer")
  "pd" '(project-dired           :which-key "dired root")
  "pc" '(project-compile         :which-key "compile")
  "pr" '(project-query-replace-regexp :which-key "query replace")
  "ps" '(consult-ripgrep         :which-key "ripgrep")
  "pt" '(project-eshell          :which-key "eshell")
  "pk" '(project-kill-buffers    :which-key "kill buffers")

  ;; Search
  "s"  '(:ignore t :which-key "search")
  "ss" '(consult-line            :which-key "line")
  "sS" '(consult-line-multi      :which-key "line (all buffers)")
  "so" '(consult-outline         :which-key "outline")
  "sg" '(consult-ripgrep         :which-key "ripgrep")
  "sf" '(consult-fd              :which-key "find file")
  "sm" '(consult-mark            :which-key "marks")
  "sr" '(consult-register        :which-key "registers")
  "sy" '(consult-yank-pop        :which-key "kill ring")

  ;; Toggles
  "t"  '(:ignore t :which-key "toggle")
  "tt" '(consult-theme           :which-key "theme")
  "tl" '(display-line-numbers-mode :which-key "line numbers")
  "tw" '(visual-line-mode        :which-key "visual line")
  "ts" '(flyspell-mode           :which-key "spellcheck")
  "tf" '(flymake-mode            :which-key "flymake")
  "tz" '(jem/toggle-writeroom    :which-key "focus mode")
  "t=" '(text-scale-adjust       :which-key "text scale")

  ;; Windows
  "w"  '(:ignore t :which-key "window")
  "ww" '(other-window            :which-key "other")
  "wd" '(delete-window           :which-key "delete")
  "wD" '(delete-other-windows    :which-key "delete others")
  "ws" '(split-window-below      :which-key "split below")
  "wv" '(split-window-right      :which-key "split right")
  "wh" '(evil-window-left        :which-key "left")
  "wj" '(evil-window-down        :which-key "down")
  "wk" '(evil-window-up          :which-key "up")
  "wl" '(evil-window-right       :which-key "right")
  "w=" '(balance-windows         :which-key "balance")
  "wu" '(winner-undo             :which-key "undo layout")
  "wr" '(winner-redo             :which-key "redo layout")

  ;; Quit
  "q"  '(:ignore t :which-key "quit")
  "qq" '(save-buffers-kill-terminal :which-key "quit emacs")
  "qr" '(restart-emacs           :which-key "restart emacs"))

;; Local leader for Emacs Lisp.
(jem/local-leader
  :keymaps 'emacs-lisp-mode-map
  "" nil
  "e"  '(:ignore t :which-key "eval")
  "es" '(eval-last-sexp :which-key "last sexp")
  "er" '(eval-region    :which-key "region")
  "eb" '(eval-buffer    :which-key "buffer")
  "ed" '(eval-defun     :which-key "defun")
  "c"  '(check-parens   :which-key "check parens")
  "I"  '(indent-region  :which-key "indent region")
  "h"  '(helpful-at-point :which-key "help at point"))

;;;; ------------------------------------------------- Minibuffer completion --

;; Vertico (UI) + Orderless (matching) + Marginalia (annotations) +
;; Consult (commands) + Embark (actions).  Each does one job.

(use-package vertico
  :init (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (vertico-count 15)
  (vertico-resize nil)
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-<return>" . vertico-exit-input)))

;; Show the directory of the current candidate in the prompt, and make DEL
;; delete a whole path component.
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
         ("RET"   . vertico-directory-enter)
         ("DEL"   . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  ;; `partial-completion' makes /u/s/l expand to /usr/share/lisp for files.
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (eglot (styles orderless))
                                   (eglot-capf (styles orderless)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package consult
  :bind
  (("C-s"     . consult-line)
   ("C-x b"   . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x r b" . consult-bookmark)
   ("M-y"     . consult-yank-pop)
   ("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g i"   . consult-imenu)
   ("M-g o"   . consult-outline)
   ("M-s r"   . consult-ripgrep)
   ("M-s f"   . consult-fd)
   :map minibuffer-local-map
   ("C-r"     . consult-history))
  :custom
  (consult-narrow-key "<")
  (register-preview-delay 0.3)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  ;; Don't auto-preview files that may be huge or remote; require an explicit key.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.4 any)
   consult-ripgrep consult-fd consult-recent-file
   :preview-key '(:debounce 0.2 any)))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ([remap describe-bindings] . embark-bindings))
  :custom
  ;; Let `which-key' render the action menu instead of a separate buffer.
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Edit grep results in place: `embark-export' from a consult-ripgrep, then
;; C-c C-p to make it editable, edit, C-c C-c to write it back.
(use-package wgrep
  :custom (wgrep-auto-save-buffer t))

;;;; --------------------------------------------------- In-buffer completion --

(use-package corfu
  :init (global-corfu-mode 1)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :bind (:map corfu-map
         ("C-j"   . corfu-next)
         ("C-k"   . corfu-previous)
         ("TAB"   . corfu-insert)
         ([tab]   . corfu-insert)
         ("M-d"   . corfu-popupinfo-toggle))
  :config
  (corfu-popupinfo-mode 1)
  ;; Emacs 31 draws child frames on TTY frames, so the old `corfu-terminal'
  ;; package is no longer needed for `emacs -nw'.
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :init
  ;; Order matters: earlier entries are tried first.  These are the global
  ;; fallbacks; Eglot installs its own capf buffer-locally in code buffers.
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  :custom
  (cape-dabbrev-min-length 3)
  (cape-dabbrev-check-other-buffers nil))

;;;; ------------------------------------------------------------------ Org --

(defun jem/org-file (name)
  "Return the absolute path of NAME inside `jem/agenda-dir'."
  (expand-file-name name jem/agenda-dir))

(defun jem/org-ensure-agenda-files ()
  "Create `jem/agenda-dir' and its four files if they do not exist.
Idempotent, and cheap enough to run at startup.  Without this, a fresh
checkout on a new machine gives you an agenda that errors on first use."
  (make-directory jem/agenda-dir t)
  (dolist (spec '(("inbox.org"    . "#+title: Inbox\n#+category: inbox\n\n")
                  ("todo.org"     . "#+title: Tasks\n#+category: task\n\n* Tasks\n")
                  ("projects.org" . "#+title: Projects\n#+category: project\n\n")
                  ("journal.org"  . "#+title: Journal\n#+category: journal\n\n")))
    (let ((file (jem/org-file (car spec))))
      (unless (file-exists-p file)
        (with-temp-file file (insert (cdr spec)))))))

(jem/org-ensure-agenda-files)

(defun jem/org-open-inbox ()    (interactive) (find-file (jem/org-file "inbox.org")))
(defun jem/org-open-todo ()     (interactive) (find-file (jem/org-file "todo.org")))
(defun jem/org-open-projects () (interactive) (find-file (jem/org-file "projects.org")))
(defun jem/org-open-journal ()  (interactive) (find-file (jem/org-file "journal.org")))
(defun jem/kb-dired ()          (interactive) (dired jem/kb))

(defun jem/org-agenda-dashboard ()
  "Open the `d' agenda view directly."
  (interactive)
  (org-agenda nil "d"))

(defun jem/org-mode-setup ()
  "Prose-friendly settings for Org buffers."
  (org-indent-mode 1)
  (visual-line-mode 1)
  (variable-pitch-mode 1))

(use-package org
  :ensure nil                          ; use the Org 9.8 bundled with Emacs 31
  :hook (org-mode . jem/org-mode-setup)
  :custom
  (org-directory jem/kb)
  (org-ellipsis " ▾")
  (org-startup-folded 'content)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  (org-confirm-babel-evaluate nil)
  (org-return-follows-link t)
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-image-actual-width '(600))

  ;; Links
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; --- Agenda -------------------------------------------------------------
  ;; A directory in `org-agenda-files' means "every .org file in it".  Only
  ;; the four agenda files are scanned, so agenda generation stays instant
  ;; even though the KB has 200+ notes.
  (org-agenda-files (list jem/agenda-dir))
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday nil)
  (org-agenda-start-day nil)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-tags-column 'auto)
  (org-deadline-warning-days 7)
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-reschedule 'time)

  ;; A deliberately small workflow.  The previous config had eight states
  ;; across two sequences and none of them were ever used; three active
  ;; states cover everything without making capture a decision problem.
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))
  (org-todo-keyword-faces
   '(("NEXT" . (:inherit warning :weight bold))
     ("WAIT" . (:inherit shadow :weight bold))
     ("CANCELLED" . (:inherit shadow :strike-through t))))
  (org-use-fast-todo-selection 'expert)
  (org-enforce-todo-dependencies t)

  (org-tag-alist '((:startgroup)
                   ("@home" . ?h) ("@work" . ?w) ("@errand" . ?e)
                   (:endgroup)
                   ("emacs" . ?E) ("reading" . ?r) ("idea" . ?i) ("note" . ?n)))

  ;; Refile out of the inbox into todo.org or projects.org.
  (org-refile-targets `((,(jem/org-file "todo.org")     :maxlevel . 2)
                        (,(jem/org-file "projects.org") :maxlevel . 2)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)

  :config
  ;; Everything captured lands in inbox.org and gets refiled later.  One
  ;; destination means capture never requires a filing decision up front.
  (setq org-capture-templates
        `(("t" "Task" entry (file ,(jem/org-file "inbox.org"))
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i"
           :empty-lines 1)
          ("n" "Note" entry (file ,(jem/org-file "inbox.org"))
           "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i"
           :empty-lines 1)
          ("l" "Task with link" entry (file ,(jem/org-file "inbox.org"))
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n%i"
           :empty-lines 1)
          ("j" "Journal" entry
           (file+olp+datetree ,(jem/org-file "journal.org"))
           "* %<%H:%M> %?\n%i"
           :empty-lines 1 :clock-in t :clock-resume t)
          ("m" "Meeting" entry
           (file+olp+datetree ,(jem/org-file "journal.org"))
           "* %<%H:%M> Meeting: %^{With} :meeting:\n%?"
           :empty-lines 1 :clock-in t :clock-resume t)
          ("p" "Project" entry (file ,(jem/org-file "projects.org"))
           "* %^{Project}\n:PROPERTIES:\n:CREATED: %U\n:END:\n** TODO %?"
           :empty-lines 1)))

  ;; Two views. "d" is the daily driver; "r" is the weekly review.
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-overriding-header "Today")))
            (todo "NEXT" ((org-agenda-overriding-header "Next actions")))
            (tags "+LEVEL=1+CATEGORY=\"inbox\""
                  ((org-agenda-overriding-header "Inbox — needs refiling")))))
          ("r" "Weekly review"
           ((agenda "" ((org-agenda-span 7)
                        (org-agenda-overriding-header "Coming week")))
            (todo "WAIT" ((org-agenda-overriding-header "Waiting on someone")))
            (todo "TODO" ((org-agenda-overriding-header "Backlog")
                          (org-agenda-todo-list-sublevels nil)))))))

  ;; Save Org buffers after refiling so nothing is lost to a crash.
  (advice-add 'org-refile :after #'org-save-all-org-buffers)

  ;; Babel and structure templates are loaded lazily here rather than at top
  ;; level; requiring them eagerly forces a full Org load during startup.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (shell . t) (python . t) (C . t) (lisp . t)))
  (require 'org-tempo)
  (dolist (tmpl '(("sh"  . "src shell")
                  ("el"  . "src emacs-lisp")
                  ("py"  . "src python")
                  ("go"  . "src go")
                  ("rs"  . "src rust")
                  ("cpp" . "src C++")
                  ("lisp" . "src lisp")))
    (add-to-list 'org-structure-template-alist tmpl))

  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  (global-set-key (kbd "C-c l") #'org-store-link))

;; org-modern replaces org-bullets (unmaintained since 2020) and also styles
;; tables, blocks, tags, timestamps and the agenda.
(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-star 'replace)
  (org-modern-hide-stars 'leading)
  (org-modern-table t)
  (org-modern-list '((?- . "•") (?+ . "‣") (?* . "▸")))
  (org-modern-checkbox nil))

;; Reveal emphasis markers and links when the cursor is inside them, so
;; `org-hide-emphasis-markers' doesn't make editing guesswork.
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))

;; Center prose in wide frames.
(defun jem/prose-fill ()
  "Center the current buffer's text at a readable width."
  (interactive)
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook ((org-mode markdown-mode) . jem/prose-fill))

(defun jem/toggle-writeroom ()
  "Toggle a distraction-free width for the current buffer."
  (interactive)
  (if (bound-and-true-p visual-fill-column-mode)
      (visual-fill-column-mode -1)
    (jem/prose-fill)))

;; Org local leader.  Note that each key appears exactly once -- the previous
;; config bound "c" twice (capture, then the clocking prefix), so `,c' for
;; capture silently never worked.
(jem/local-leader
  :keymaps 'org-mode-map
  "" nil
  "a" '(org-archive-subtree-default :which-key "archive")
  "d" '(org-deadline          :which-key "deadline")
  "e" '(org-export-dispatch   :which-key "export")
  "g" '(consult-org-heading   :which-key "goto heading")
  "l" '(org-insert-link       :which-key "insert link")
  "n" '(org-narrow-to-subtree :which-key "narrow")
  "N" '(widen                 :which-key "widen")
  "p" '(org-set-property      :which-key "property")
  "r" '(org-refile            :which-key "refile")
  "s" '(org-schedule          :which-key "schedule")
  "t" '(org-todo              :which-key "todo state")
  "T" '(org-set-tags-command  :which-key "set tags")
  "S" '(org-sort              :which-key "sort")
  "x" '(org-toggle-checkbox   :which-key "toggle checkbox")
  "b" '(:ignore t             :which-key "babel")
  "bb" '(org-edit-special     :which-key "edit block")
  "bt" '(org-babel-tangle     :which-key "tangle")
  "be" '(org-babel-execute-src-block :which-key "execute")
  "c" '(:ignore t             :which-key "clock")
  "ci" '(org-clock-in         :which-key "clock in")
  "co" '(org-clock-out        :which-key "clock out")
  "cj" '(org-clock-goto       :which-key "goto clock")
  "cr" '(org-clock-report     :which-key "clock report")
  "i" '(:ignore t             :which-key "insert")
  "it" '(org-table-create     :which-key "table")
  "ih" '(org-table-insert-hline :which-key "table hline")
  "id" '(org-time-stamp       :which-key "timestamp"))

(jem/local-leader
  :keymaps 'org-agenda-mode-map
  "" nil
  "d" '(org-agenda-deadline  :which-key "deadline")
  "s" '(org-agenda-schedule  :which-key "schedule")
  "t" '(org-agenda-todo      :which-key "todo state")
  "T" '(org-agenda-set-tags  :which-key "set tags")
  "r" '(org-agenda-refile    :which-key "refile")
  "ci" '(org-agenda-clock-in :which-key "clock in")
  "co" '(org-agenda-clock-out :which-key "clock out"))

;;;; ------------------------------------------------- Denote (org KB notes) --

;; Denote names each note <id>--<title>__<keywords>.org, so the filesystem is
;; the database.  It coexists with the 200+ hand-named .org files already in
;; the KB: non-conforming names are simply ignored by Denote's own commands,
;; and `consult-denote' still greps the whole directory.

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :custom
  (denote-directory jem/kb)
  (denote-file-type 'org)
  (denote-known-keywords '("emacs" "project" "journal" "reference" "idea"
                           "coding" "cpp" "python" "golang" "rust" "lisp"))
  (denote-prompts '(title keywords))
  (denote-date-prompt-use-org-read-date t)
  (denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  :config
  ;; Make `denote-link' and friends available in Org buffers anywhere.
  (denote-rename-buffer-mode 1))

(use-package denote-org
  :after (denote org))

;; consult-denote overrides denote's find/grep with live-preview versions.
(use-package consult-denote
  :after (consult denote)
  :config (consult-denote-mode 1))

;;;; ------------------------------------------------------ Markdown notes --

;; The md notes are a flat directory of hand-named files with a MOC.md index --
;; a different system from the Org KB, kept deliberately separate.

(defun jem/md-find-file ()
  "Find a file in `jem/md'."
  (interactive)
  (let ((default-directory jem/md))
    (call-interactively #'find-file)))

(defun jem/md-grep ()
  "Ripgrep across `jem/md'."
  (interactive)
  (consult-ripgrep jem/md))

(defun jem/md-dired ()
  "Open `jem/md' in Dired."
  (interactive)
  (dired jem/md))

(defun jem/md-open-moc ()
  "Open the markdown map-of-content index."
  (interactive)
  (find-file (expand-file-name "MOC.md" jem/md)))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "pandoc")          ; only needed for C-c C-c p preview
  (markdown-enable-wiki-links t)
  (markdown-wiki-link-search-subdirectories t)
  (markdown-enable-math t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-urls nil)
  (markdown-asymmetric-header t)
  (markdown-header-scaling t)
  :config
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook #'variable-pitch-mode)
  ;; Keep code and tables monospaced even with variable-pitch on.
  (dolist (face '(markdown-code-face markdown-inline-code-face
                  markdown-pre-face markdown-table-face
                  markdown-language-keyword-face markdown-url-face))
    (set-face-attribute face nil :inherit 'fixed-pitch)))

;; Table-of-contents generation lives in its own package, not markdown-mode.
(use-package markdown-toc
  :after markdown-mode
  :commands (markdown-toc-generate-toc markdown-toc-generate-or-refresh-toc)
  :custom (markdown-toc-header-toc-title "**Table of Contents**"))

(jem/local-leader
  :keymaps '(markdown-mode-map gfm-mode-map)
  "" nil
  "b" '(markdown-insert-bold        :which-key "bold")
  "i" '(markdown-insert-italic      :which-key "italic")
  "c" '(markdown-insert-code        :which-key "inline code")
  "C" '(markdown-insert-gfm-code-block :which-key "code block")
  "l" '(markdown-insert-link        :which-key "link")
  "L" '(markdown-insert-wiki-link   :which-key "wiki link")
  "h" '(markdown-insert-header-dwim :which-key "header")
  "-" '(markdown-insert-hr          :which-key "horizontal rule")
  "t" '(markdown-toc-generate-or-refresh-toc :which-key "table of contents")
  "p" '(markdown-preview            :which-key "preview")
  "o" '(markdown-follow-thing-at-point :which-key "follow link")
  "n" '(markdown-narrow-to-subtree  :which-key "narrow")
  "N" '(widen                       :which-key "widen")
  "x" '(markdown-toggle-gfm-checkbox :which-key "toggle checkbox")
  "'" '(markdown-edit-code-block    :which-key "edit code block"))

;;;; ---------------------------------------------------------------- Elfeed --

;; Feeds are defined in KB/emacs/elfeed.org, not here, so adding a feed is a
;; note edit rather than a config change.  Format: a heading whose title is an
;; org link subscribes to that feed; org tags on ancestor headings are
;; inherited as elfeed tags.

(use-package elfeed
  :commands (elfeed elfeed-update)
  :bind ("C-c e" . elfeed)
  :custom
  (elfeed-search-filter "@2-weeks-ago +unread")
  (elfeed-search-title-max-width 100)
  (elfeed-db-directory (expand-file-name "elfeed/db" user-emacs-directory))
  (elfeed-enclosure-default-dir (expand-file-name "Downloads/" jem/home))
  :config
  (elfeed-set-max-connections 32)
  ;; Open entries in eww by default; `b' still opens the system browser.
  (setq elfeed-show-entry-switch #'pop-to-buffer-same-window))

(use-package elfeed-org
  :after elfeed
  :custom
  (rmh-elfeed-org-files (list (expand-file-name "emacs/elfeed.org" jem/kb)))
  :config
  (elfeed-org)
  ;; elfeed-org only reads the org file when it is loaded, so re-read it every
  ;; time the search buffer is opened -- otherwise edits to elfeed.org need an
  ;; Emacs restart to take effect.
  (advice-add 'elfeed :before #'elfeed-org))

(jem/local-leader
  :keymaps 'elfeed-search-mode-map
  "" nil
  "u" '(elfeed-update              :which-key "fetch all feeds")
  "f" '(elfeed-search-live-filter  :which-key "filter")
  "c" '(elfeed-search-clear-filter :which-key "clear filter")
  "r" '(elfeed-search-untag-all-unread :which-key "mark read")
  "U" '(elfeed-search-tag-all-unread   :which-key "mark unread")
  "b" '(elfeed-search-browse-url   :which-key "open in browser")
  "g" '(elfeed-search-update--force :which-key "refresh view")
  "o" '(jem/elfeed-open-feed-file  :which-key "edit elfeed.org"))

(defun jem/elfeed-open-feed-file ()
  "Open the org file that defines the elfeed subscriptions."
  (interactive)
  (find-file (expand-file-name "emacs/elfeed.org" jem/kb)))

;;;; ------------------------------------------------------- Tree-sitter --

;; Emacs 31 ships tree-sitter and a *-ts-mode for every language used here.
;; Grammars are compiled per machine and are not bundled, so `M-x
;; jem/treesit-install-grammars' is a one-time setup step (needs a C compiler
;; and git).  Until a grammar is installed the classic mode is used, so a
;; fresh machine degrades gracefully instead of erroring.

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)          ; richest highlighting
  :config
  (setq treesit-language-source-alist
        '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
          (c          "https://github.com/tree-sitter/tree-sitter-c")
          (cpp        "https://github.com/tree-sitter/tree-sitter-cpp")
          (cmake      "https://github.com/uyha/tree-sitter-cmake")
          (go         "https://github.com/tree-sitter/tree-sitter-go")
          (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
          (json       "https://github.com/tree-sitter/tree-sitter-json")
          (markdown   "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                      "split_parser" "tree-sitter-markdown/src")
          (python     "https://github.com/tree-sitter/tree-sitter-python")
          (rust       "https://github.com/tree-sitter/tree-sitter-rust")
          (toml       "https://github.com/tree-sitter/tree-sitter-toml")
          (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

  (defun jem/treesit-languages ()
    "Return the de-duplicated list of languages Emacs knows a source for.
Emacs\' own `*-ts-mode' files each `add-to-list' their grammar sources when
loaded, so this alist normally contains duplicate keys; `assq' lookup takes
the first, but iterating the keys would otherwise visit some twice."
    (delete-dups (mapcar #'car treesit-language-source-alist)))

  (defun jem/treesit-install-grammars (&optional force)
    "Install every known tree-sitter grammar that is missing.
With a prefix argument FORCE, reinstall grammars that are already present.
Run once per machine; requires git and a C compiler."
    (interactive "P")
    (let ((langs (jem/treesit-languages))
          (installed 0) (skipped 0) (failed '()))
      (dolist (lang langs)
        (if (and (treesit-language-available-p lang) (not force))
            (setq skipped (1+ skipped))
          (message "tree-sitter: installing %s..." lang)
          (condition-case err
              (progn (treesit-install-language-grammar lang)
                     (setq installed (1+ installed)))
            (error (push (cons lang (error-message-string err)) failed)))))
      (message "tree-sitter: %d installed, %d already present, %d failed%s"
               installed skipped (length failed)
               (if failed
                   (concat " -- " (mapconcat (lambda (f) (format "%s" (car f)))
                                             (nreverse failed) " "))
                 ""))))

  ;; Remap to the ts mode only when its grammar is actually present.
  (dolist (entry '((c-mode          . (c          . c-ts-mode))
                   (c++-mode        . (cpp        . c++-ts-mode))
                   (c-or-c++-mode   . (cpp        . c-or-c++-ts-mode))
                   (python-mode     . (python     . python-ts-mode))
                   (sh-mode         . (bash       . bash-ts-mode))
                   (js-mode         . (javascript . js-ts-mode))
                   (json-mode       . (json       . json-ts-mode))
                   (conf-toml-mode  . (toml       . toml-ts-mode))
                   (yaml-mode       . (yaml       . yaml-ts-mode))
                   (cmake-mode      . (cmake      . cmake-ts-mode))))
    (let ((lang (car (cdr entry)))
          (from (car entry))
          (to   (cdr (cdr entry))))
      (when (and (treesit-language-available-p lang) (fboundp to))
        (add-to-list 'major-mode-remap-alist (cons from to)))))

  ;; Go and Rust have no classic built-in mode, so bind the ts modes to the
  ;; file extensions directly when their grammars exist.
  (when (treesit-language-available-p 'go)
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode)))
  (when (treesit-language-available-p 'gomod)
    (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode)))
  (when (treesit-language-available-p 'rust)
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode)))

  ;; Say so at startup when grammars are missing, rather than leaving it to be
  ;; discovered by opening a .go file and getting fundamental-mode.  Without a
  ;; grammar, Go and Rust have no major mode at all -- there is no classic
  ;; go-mode or rust-mode built into Emacs to fall back to.
  (defun jem/treesit-report-missing-grammars ()
    (let ((missing (seq-remove #'treesit-language-available-p
                                 (jem/treesit-languages))))
      (when missing
        (message "tree-sitter: %d grammar(s) missing (%s%s). Run M-x jem/treesit-install-grammars"
                 (length missing)
                 (mapconcat #'symbol-name (seq-take missing 4) " ")
                 (if (> (length missing) 4) " ..." "")))))
  (add-hook 'emacs-startup-hook #'jem/treesit-report-missing-grammars 90))

;;;; ----------------------------------------------------------- Eglot (LSP) --

;; Eglot is built in.  It uses the facilities already configured above --
;; capf for completion (Corfu), xref for navigation (Consult), Flymake for
;; diagnostics -- so it needs almost no configuration.
;;
;; Language servers must be installed separately:
;;   C/C++   clangd            brew install llvm   (or the distro package)
;;   Python  pylsp             pipx install "python-lsp-server[all]"
;;   Go      gopls             go install golang.org/x/tools/gopls@latest
;;   Rust    rust-analyzer     rustup component add rust-analyzer
;;   Common Lisp uses sly, not LSP.

(use-package eglot
  :ensure nil
  :hook ((c-ts-mode c++-ts-mode c-mode c++-mode
          python-ts-mode python-mode
          go-ts-mode rust-ts-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)               ; stop the server with the last buffer
  (eglot-sync-connect 1)
  (eglot-connect-timeout 20)
  (eglot-events-buffer-size 0)         ; the event log is a real memory cost
  (eglot-extend-to-xref t)
  :config
  ;; Don't let Eglot take over the whole modeline.
  (setq eglot-report-progress nil)

  ;; clangd flags that matter in practice: use the compile_commands.json in
  ;; build/, and let it index the whole project in the background.
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode)
                 . ("clangd"
                    "--background-index"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--header-insertion=never"
                    "--compile-commands-dir=build")))

  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) . ("rust-analyzer")))

  ;; Format Go and Rust on save -- both ecosystems assume it.
  (defun jem/eglot-format-on-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer nil 'local))
  (dolist (hook '(go-ts-mode-hook rust-ts-mode-hook))
    (add-hook hook #'jem/eglot-format-on-save))

  ;; goimports-style organize-imports on save for Go.
  (defun jem/go-organize-imports ()
    (when (eglot-managed-p)
      (ignore-errors (eglot-code-action-organize-imports (point-min) (point-max)))))
  (add-hook 'go-ts-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'jem/go-organize-imports nil 'local))))

;; Workspace-wide symbol search with live preview.
(use-package consult-eglot
  :after eglot)

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-fringe-indicator-position 'right-fringe)
  :bind (:map flymake-mode-map
         ("C-c ! n" . flymake-goto-next-error)
         ("C-c ! p" . flymake-goto-prev-error)
         ("C-c ! l" . consult-flymake)))

;;;; -------------------------------------------------------------- Languages --

(use-package c-ts-mode
  :ensure nil
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'linux)
  :config
  ;; The previous config's compile-command assumed a src/ + build/ layout;
  ;; this one works from anywhere in a CMake project.
  (defun jem/c-compile-command ()
    (setq-local compile-command
                (if (locate-dominating-file default-directory "CMakeLists.txt")
                    "cmake --build build"
                  (format "make -k %s"
                          (or (and buffer-file-name
                                   (file-name-base buffer-file-name))
                              "")))))
  (add-hook 'c-ts-mode-hook #'jem/c-compile-command)
  (add-hook 'c++-ts-mode-hook #'jem/c-compile-command))

(use-package python
  :ensure nil                          ; built-in python.el, NOT the
                                       ; third-party `python-mode' package
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3")
  :config
  (defun jem/python-compile-command ()
    (setq-local compile-command
                (concat "python3 "
                        (if buffer-file-name
                            (shell-quote-argument buffer-file-name)
                          ""))))
  (add-hook 'python-base-mode-hook #'jem/python-compile-command))

;; Activate the project's virtualenv so Eglot's pylsp sees the right packages.
(use-package pyvenv
  :hook (python-base-mode . pyvenv-mode)
  :commands (pyvenv-activate pyvenv-workon))

(use-package go-ts-mode
  :ensure nil
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  (add-to-list 'exec-path (expand-file-name "go/bin" jem/home)))

(use-package rust-ts-mode
  :ensure nil
  :config
  (defun jem/rust-compile-command ()
    (setq-local compile-command "cargo check"))
  (add-hook 'rust-ts-mode-hook #'jem/rust-compile-command))

;; Common Lisp.  sly is a SLIME fork with a better REPL and stickers.
(use-package sly
  :commands (sly sly-connect)
  :custom
  (sly-net-coding-system 'utf-8-unix)
  :config
  ;; Find sbcl wherever it lives: /opt/homebrew/bin on Apple Silicon,
  ;; /usr/local/bin on Intel, /usr/bin on most Linux distributions.
  (setq inferior-lisp-program (or (executable-find "sbcl") "sbcl")))

(use-package sly-quicklisp
  :after sly)

(jem/local-leader
  :keymaps '(lisp-mode-map sly-mrepl-mode-map)
  "" nil
  "'" '(sly                    :which-key "start repl")
  "e" '(:ignore t              :which-key "eval")
  "ee" '(sly-eval-last-expression :which-key "last expression")
  "ed" '(sly-eval-defun        :which-key "defun")
  "eb" '(sly-eval-buffer       :which-key "buffer")
  "er" '(sly-eval-region       :which-key "region")
  "c" '(sly-compile-defun      :which-key "compile defun")
  "C" '(sly-compile-and-load-file :which-key "compile file")
  "d" '(sly-describe-symbol    :which-key "describe symbol")
  "h" '(sly-documentation-lookup :which-key "hyperspec")
  "m" '(sly-macroexpand-1      :which-key "macroexpand"))

;;;; ---------------------------------------------------------------- Compile --

(use-package compile
  :ensure nil
  :bind (("C-x M-m" . compile)
         ("C-x C-m" . recompile))
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  :config
  ;; Render ANSI colour codes in the compilation buffer rather than showing
  ;; the raw escape sequences.
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;;;; -------------------------------------------------------- Project & Git --

;; project.el is built in and is what Eglot, xref and Flymake already use.
(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers '(".project.el" "Cargo.toml" "go.mod"
                                   "CMakeLists.txt" "pyproject.toml"))
  :config
  (setq project-switch-commands
        '((project-find-file "Find file" ?f)
          (consult-ripgrep "Ripgrep" ?s)
          (magit-project-status "Magit" ?g)
          (project-dired "Dired" ?d)
          (project-eshell "Eshell" ?e))))

(use-package magit
  :commands (magit-status magit-file-dispatch magit-project-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk 'all)
  (magit-save-repository-buffers 'dontask))

;; Forge needs a GitHub token in ~/.authinfo.gpg before first use:
;; https://magit.vc/manual/forge/Token-Creation.html
(use-package forge
  :after magit
  ;; evil-collection installs its own forge bindings and sets this to nil
  ;; anyway, printing a warning if we leave it at the default.  Set it here
  ;; so the two agree and startup stays quiet.
  :custom (forge-add-default-bindings nil))

;;;; ----------------------------------------------------------------- Dired --

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind ("C-x C-j" . dired-jump)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-dwim-target t)                ; default copy target = other window
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-kill-when-opening-new-dired-buffer t)   ; no buffer-per-directory
  (dired-auto-revert-buffer t)
  (dired-free-space nil)
  :config
  ;; macOS ships BSD ls, which understands neither `--group-directories-first'
  ;; nor `--dired'.  When ls errors, Emacs 31 kills the Dired buffer and
  ;; returns nil, so `find-file' on a directory fails outright.  Prefer GNU ls
  ;; (brew install coreutils), else fall back to Emacs' own ls-lisp, which
  ;; does directory grouping itself and needs no GNU-only switch.
  (when (eq system-type 'darwin)
    (if (executable-find "gls")
        (setq insert-directory-program "gls")
      (require 'ls-lisp)
      (setq ls-lisp-use-insert-directory-program nil
            ls-lisp-dirs-first t
            dired-listing-switches "-agho")))

  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" #'dired-up-directory
      "l" #'dired-find-file
      "H" #'dired-hide-dotfiles-mode)))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired-hide-dotfiles
  :commands dired-hide-dotfiles-mode
  :hook (dired-mode . dired-hide-dotfiles-mode))

;;;; ------------------------------------------------------------- Terminals --

(use-package vterm
  ;; Requires a compiled native module; on first run Emacs offers to build it.
  ;; Needs cmake and libtool: brew install cmake libtool
  :commands vterm
  :custom
  (vterm-max-scrollback 10000)
  (vterm-timer-delay 0.01)
  :config
  ;; vterm handles its own keys; evil normal state would swallow them.
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil))))

(use-package eshell
  :ensure nil
  :commands eshell
  :custom
  (eshell-history-size 10000)
  (eshell-buffer-maximum-lines 10000)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-input 'this)
  (eshell-destroy-buffer-when-process-dies t)
  :config
  (defun jem/eshell-setup ()
    (add-hook 'eshell-pre-command-hook #'eshell-save-some-history nil t)
    (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
    (keymap-set eshell-mode-map "C-r" #'consult-history))
  (add-hook 'eshell-first-time-mode-hook #'jem/eshell-setup))

;;;; ------------------------------------------------------------ Spellcheck --

;; Requires aspell (brew install aspell / dnf install aspell-en).
(defconst jem/hunspell-dictionary-dirs
  '("/opt/homebrew/share/hunspell" "/usr/local/share/hunspell"
    "/usr/share/hunspell" "/usr/share/myspell" "/usr/share/myspell/dicts"
    "/Library/Spelling" "~/Library/Spelling" "~/.local/share/hunspell")
  "Standard locations for hunspell dictionaries.")

(defun jem/hunspell-has-dictionary-p ()
  "Return non-nil if a hunspell dictionary (.aff file) exists on disk.

This is deliberately a filesystem check rather than parsing `hunspell -D'.
Running that as a subprocess does not terminate under a GUI Emacs on macOS
-- it blocks in `call-process' forever and hangs startup with a blank frame,
even with /dev/null on stdin.  Nothing here may spawn a process: this runs on
every launch, before any frame is usable."
  (seq-some (lambda (dir)
              (let ((d (expand-file-name dir)))
                (and (file-directory-p d)
                     (directory-files d nil "\\.aff\\'" t 1))))
            jem/hunspell-dictionary-dirs))

(defvar jem/spell-program
  (let ((aspell (executable-find "aspell"))
        (hunspell (executable-find "hunspell")))
    (cond (aspell aspell)
          ((and hunspell (jem/hunspell-has-dictionary-p)) hunspell)))
  "Spell checker binary, or nil if none is installed with a usable dictionary.
On macOS: brew install aspell")

(use-package flyspell
  :ensure nil
  ;; Only hook flyspell in when a checker actually exists, so a machine
  ;; without one gets no errors -- just no spellcheck.
  :if jem/spell-program
  :hook ((org-mode markdown-mode) . flyspell-mode)
  :custom
  (ispell-dictionary "en_US")
  (flyspell-issue-message-flag nil)    ; the messages are a typing slowdown
  :config
  ;; Plain `setq', not `:custom': the defcustom setter validates the
  ;; dictionary eagerly and signals during startup on a bad install.
  (setq ispell-program-name jem/spell-program)
  ;; Don't spellcheck code, markup or property drawers.
  (dolist (region '(("~" . "~")
                    ("=" . "=")
                    ("^#\\+BEGIN_SRC"    . "^#\\+END_SRC")
                    ("^#\\+BEGIN_EXPORT" . "^#\\+END_EXPORT")
                    ("^```"              . "^```")
                    (":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")))
    (add-to-list 'ispell-skip-region-alist region))
  ;; Correction on right-click rather than middle-click.
  (keymap-set flyspell-mouse-map "<mouse-3>" #'flyspell-correct-word)
  (keymap-set flyspell-mouse-map "<mouse-2>" nil))

;;;; -------------------------------------------------------------- Web / eww --

(use-package eww
  :ensure nil
  :commands eww
  :bind ("C-x w w" . eww)
  :custom
  (eww-search-prefix "https://duckduckgo.com/html/?q=")
  (shr-use-colors nil)
  (shr-max-image-proportion 0.6)
  (shr-width 90))

;;;; -------------------------------------------------------------------- AI --

;; gptel against a local Ollama instance.  `M-x gptel-menu' (or SPC a g) sets
;; the backend and model interactively; change the default below to whatever
;; `ollama list' reports.
(use-package gptel
  :commands (gptel gptel-send gptel-menu)
  :config
  (setq gptel-default-mode 'org-mode
        gptel-include-reasoning nil)
  (let ((model 'gemma3:latest))
    (setq gptel-backend (gptel-make-ollama "Ollama"
                          :host "localhost:11434"
                          :stream t
                          :models (list model))
          gptel-model model)))

;;;; ----------------------------------------------------------- Local files --

;; Hand-written elisp that is not a package.  `bark.el' stores URLs in a plain
;; file (distinct from Emacs' own bookmarks).
(let ((bark (expand-file-name "bark.el" jem/lisp-dir)))
  (when (file-exists-p bark)
    (load bark nil 'nomessage)))

;; Settings written by the Customize interface.  Loaded last so it wins over
;; everything above; :noerror because it does not exist on a fresh machine.
(load custom-file 'noerror 'nomessage)

(provide 'init)
;;; init.el ends here
