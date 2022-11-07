;;; -*- lexical-binding: t -*-

;; Debug
;(setq init-file-debug t)

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
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(inhibit-double-buffering . t) default-frame-alist) ;; RealVNC

(setq load-prefer-newer t)
