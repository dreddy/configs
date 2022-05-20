;;; Install into separate package dirs for each Emacs version,
;;; to use a single filesystem on multiple environments
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s" emacs-major-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

