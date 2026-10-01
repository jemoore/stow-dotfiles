;;; early-init.el --- Loaded before the UI and package system -*- lexical-binding: t; -*-

;; Emacs reads this file before `package-initialize' and before the first frame
;; is created.  Anything that affects startup cost or causes a visible flash
;; belongs here, not in init.el.

;;; Code:

;; Raise the GC threshold for the duration of startup.  Loading a config
;; allocates heavily, and collecting during that is pure waste.  `jem/gc-restore'
;; in init.el puts it back to a sane steady-state value.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Don't let package.el activate packages before init.el runs; init.el does it
;; explicitly so the ordering is visible in one place.
(setq package-enable-at-startup nil)

;; Prefer newer .el over stale .elc when both exist.
(setq load-prefer-newer t)

;; Frame chrome: set these in `default-frame-alist' rather than calling
;; `tool-bar-mode' etc. later.  Toggling modes after the frame exists makes the
;; frame visibly resize twice during startup.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Don't let Emacs resize the frame to fit changing font/UI metrics at startup.
(setq frame-inhibit-implied-resize t)

;; Startup screen and scratch message.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

;; Native compilation is on in this build; keep its warnings out of the way.
;; They are almost always about third-party packages you cannot fix.
(setq native-comp-async-report-warnings-errors 'silent
      native-comp-jit-compilation t)

;; Emacs 31 warns for every loaded file whose line 1 lacks a `lexical-binding'
;; cookie.  For third-party packages that is not actionable: the only offender
;; here is sly-quicklisp (last released 2021), and the warning names a
;; *generated* autoloads file, so the suggested `M-x elisp-enable-lexical-binding'
;; would be undone the next time the package is rebuilt.
;;
;; This must run before `package-initialize' loads any package autoloads,
;; which is why it lives in early-init.el rather than init.el.
;;
;; `warning-suppress-types' (not `-log-types') stops the *Warnings* buffer from
;; popping up while still recording the warning there, so it stays discoverable.
(require 'warnings)
(add-to-list 'warning-suppress-types '(files missing-lexbind-cookie))

;; `file-name-handler-alist' is consulted for every `load' and `require'.
;; Emptying it during startup is a measurable win; init.el restores it.
(defvar jem/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(provide 'early-init)
;;; early-init.el ends here
