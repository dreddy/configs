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
              (when custom-file
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
      frame-inhibit-implied-resize t)

;; Platform specific settings
(push '(background-color . "#123") default-frame-alist)
(cond ((eq system-type 'windows-nt)
       (setq inhibit-compacting-font-caches t)
       (push '(font . "Consolas-12") default-frame-alist))
      ((eq system-type 'darwin)
       (setq mac-command-modifier 'meta
             mac-option-modifier 'super)
       (setq-default line-spacing 1)
       (push '(font . "Menlo-12") default-frame-alist))
      ((eq system-type 'gnu/linux)
       (push '(inhibit-double-buffering . t) default-frame-alist))
      ) ;; end cond
