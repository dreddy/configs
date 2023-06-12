;;; -*- lexical-binding: t -*-

;; Debug
(setq init-file-debug t)

;; Init Speedup
(let ((default-fnh-list file-name-handler-alist)
      (default-gc-threshold gc-cons-threshold)
      (default-gc-percentage gc-cons-percentage))
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.8)
  (add-hook 'after-init-hook
            (lambda ()
              (message "Emacs ready in %.2f secs with %d garbage collections."
                       (float-time
                        (time-subtract after-init-time before-init-time))
                       gcs-done)
              (setq file-name-handler-alist default-fnh-list
                    gc-cons-threshold default-gc-threshold
                    gc-cons-percentage default-gc-percentage)
              (garbage-collect))))

;; Faster to disable these here (before they've been initialized)
(dolist (mode '(tool-bar-mode tooltip-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

(setq frame-resize-pixelwise t)
(push '(background-color . "#123") default-frame-alist)

;; Platform specific settings
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

(setq
 load-prefer-newer t
 create-lockfiles nil)
