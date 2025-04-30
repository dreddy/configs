;; init.el -*- lexical-binding: t; -*-

(when (< emacs-major-version 27)
  (let ((ifile (locate-user-emacs-file "early-init.el")))
    (when (file-exists-p ifile) (load-file ifile))))

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

(setq-default
 package-native-compile t
 use-package-always-defer t
 use-package-always-ensure t)

(require 'package)
;(package-initialize)

(add-to-list 'package-archives
       '("melpa"  . "https://melpa.org/packages/"))

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

;; Enable these
(dolist (c '(list-timers narrow-to-region narrow-to-page
			 upcase-region downcase-region))
  (put c 'disabled nil))

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(setq-default
 sentence-end-double-space nil
 show-paren-style 'mixed
 indent-tabs-mode nil
 tab-always-indent 'complete
 indicate-empty-lines t
 fill-column 80
 line-number-mode t
 column-number-mode t
 c-basic-offset 4
 tab-width 8
 word-wrap 1)

(define-key global-map (kbd "M-g") 'goto-line)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq backup-by-copying t)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(when (boundp 'mac-pass-command-to-system)
  (setq mac-pass-command-to-system nil))

(use-package emacs
  :config
  (load-theme 'wombat))

(use-package prog-mode
  :hook ((prog-mode . show-paren-mode)
         (prog-mode . electric-pair-mode)
         ;;         (prog-mode . auto-insert-mode)
         (prog-mode . auto-fill-mode)
         (prog-mode . electric-indent-mode)
         (prog-mode . hs-minor-mode))
  :bind
  ("<backtab>" . hs-toggle-hiding)
  :config
  (setq display-line-numbers-type 'relative))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq
   python-shell-interpreter "python3"
   python-indent-offset 4
   python-indent-guess-indent-offset-verbose nil)
   (add-hook 'python-mode-hook 'hs-minor-mode))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package ggtags
    :commands ggtags-mode
    :config
    (unbind-key "M-<" ggtags-mode-map)
    (unbind-key "M->" ggtags-mode-map))

(use-package org
  :ensure nil
  :config
  (setq
     org-log-done t
     org-src-fontify-natively t
     org-startup-indented t
     org-startup-folded nil

     org-html-doctype "html5"
     org-export-with-toc nil
     org-export-with-author t
     org-export-with-email nil
     org-export-with-creator nil
     org-export-with-section-numbers t

     org-html-preamble nil
     org-html-postamble nil )
  )

(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let ((homestyle "~/.config/emacs/org-style.css")) ;; <- set your own style
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents homestyle)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))


(add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)

(setq auth-source-debug t)
(setq auth-sources '("~/.authinfo"))

(use-package gptel
  :config
  (setq-default
   gptel-model 'gemini-2.5-pro-exp-03-25
   gptel-backend (gptel-make-gemini "Gemini"
                   :stream t
                   :key gptel-api-key)
                  ;; :key (gptel-api-key-from-auth-source "generativelanguage.googleapis.com"))

   ))
