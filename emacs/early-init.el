;;; Install into separate package dirs for each Emacs version,
;;; to use a single filesystem on multiple environments
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s" emacs-major-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(inhibit-double-buffering . t) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
