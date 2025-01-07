;;; -*- lexical-binding: t -*-

;; Debug
(setq init-file-debug t)

(setq-default
 load-prefer-newer t
 create-lockfiles nil)

;; Init Speedup
(let ((default-fnh-list file-name-handler-alist)
      (default-gc-threshold gc-cons-threshold)
      (default-vc-handled-backends vc-handled-backends))
  (setq gc-cons-threshold most-positive-fixnum
        file-name-handler-alist nil
        vc-handled-backends nil)

  (add-hook 'after-init-hook
            (lambda ()
              (when (file-exists-p custom-file) ; Don’t forget to load it, we still need it
                (load-file custom-file))
              (message "Started in %s %d GCs" (emacs-init-time) gcs-done)
              (setq file-name-handler-alist default-fnh-list
                    gc-cons-threshold default-gc-threshold
                    vc-handled-backends default-vc-handled-backends))
            ))

;; Faster to disable these here (before they've been initialized)
(dolist (mode '(tool-bar-mode tooltip-mode scroll-bar-mode blink-cursor-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-x-resources t
      initial-scratch-message nil
      custom-file (locate-user-emacs-file "custom.el"))

;; Contrary to common configurations, this is all that's needed to set UTF-8
;; as the default coding system:
(set-language-environment "UTF-8")

;; Initialise installed packages at this early stage, by using the
;; available cache. From _prot_ setting this to nil had the following effect
;; (i) it ended up being slower and (ii) various
;; package commands, like `describe-package', did not have an index of
;; packages to work with, requiring a `package-refresh-contents'.
;; (setq package-enable-at-startup t)


;; Platform specific settings
(cond ((eq system-type 'windows-nt)
       (setq inhibit-compacting-font-caches t)
       (push '(font . "Consolas-12") default-frame-alist))
      ((eq system-type 'darwin)
       (setq mac-command-modifier 'meta
             mac-option-modifier nil)
       (setq-default line-spacing 1)
       (push '(font . "Menlo-12") default-frame-alist))
      ((eq system-type 'gnu/linux)
       (push '(inhibit-double-buffering . t) default-frame-alist))
      ) ;; end cond
