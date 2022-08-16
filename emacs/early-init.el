(defconst emacs-start-time (current-time))

;; Uncomment this to debug.
(setq init-file-debug t)
(setq messages-buffer-max-lines 100000)

;; If an `.el' file is newer than its corresponding `.elc', load the `.el'.
(setq load-prefer-newer t)

;; Set Garbage Collection threshold to 1GB during startup. `gcmh' will clean
;; things up later.
(setq gc-cons-threshold 1073741824
      gc-cons-percentage 0.6)

;; Faster to disable these here (before they've been initialized)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(inhibit-double-buffering . t) default-frame-alist) ;; Needed for some VNC servers
(push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)

