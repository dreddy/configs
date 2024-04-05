;;-*- lexical-binding: t -*-


(when (version< emacs-version "27.0")
  (let ((dafile (expand-file-name "early-init.el"  user-emacs-directory)))
                  (when (file-exists-p dafile) (load-file dafile))))

;; Set proxy for work machines (EC in pdx,sc,sj and personal machines)
(if (string-match "\\(^dreddy\\|^plx\\|^sc\\|^sj\\)" (system-name))
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10.*\\|134.134.*\\|*.intel.com\\)")
          ("http" . "proxy-dmz.intel.com:912")
          ("https" . "proxy-dmz.intel.com:912")
          ("socks" . "proxy-dmz.intel.com:1080")
          )))

;; Package configuration and use-package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(when (version< emacs-version "27.0") (package-initialize))
(setq-default use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq-default
 use-package-always-defer t ; Always derger loading packages
 use-package-verbose nil ; Don't report loading details
 use-package-expand-minimally t)  ; make the expanded code as minimal as possible

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Enable these
(dolist (c '( narrow-to-region narrow-to-page upcase-region downcase-region))
  (put c 'disabled nil))
;; Sane behaviors
(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook (show-paren-mode t))
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook (auto-insert-mode t))

;; Buffer encoding
(set-language-environment   "UTF-8")

;; Default indentation
(setq-default
 indent-tabs-mode nil
 indicate-empty-lines t
 fill-column 80
 line-number-mode t
 column-number-mode t
 c-basic-offset 4
 tab-width 8
 word-wrap 1)


(setq
 show-paren-style 'mixed
 inhibit-startup-screen t
 initial-scratch-message ";; "
 blink-cursor-mode nil
 use-short-answers t
 unibyte-display-via-language-environment t
 auto-save-default nil
 select-enable-primary t
 kill-ring-max 128
 mark-ring-max 128
 find-file-visit-truename t
 vc-follow-symlinks t
 custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (boundp 'mac-pass-command-to-system)
  (setq mac-pass-command-to-system nil))

(setq backup-directory-alist ; Backup directory
      `(("." .
         ,(expand-file-name "backups" user-emacs-directory))))


(add-hook 'before-save-hook 'delete-trailing-whitespace)

(transient-mark-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode t)

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :ensure nil
  :config
  (setq
   python-shell-interpreter "python3"
   python-indent-offset 4
   python-indent-guess-indent-offset-verbose nil)
  (add-hook 'python-mode-hook 'hs-minor-mode))

(use-package org
  :ensure nil
  :config
  (setq
   org-log-done t
   org-src-fontify-natively t
   org-startup-indented t
   org-startup-folded nil))

(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  (setq rust-indent-offset 4))

(use-package cargo-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package ggtags
    :commands ggtags-mode
    :config
    (unbind-key "M-<" ggtags-mode-map)
    (unbind-key "M->" ggtags-mode-map))

(use-package calc
  ;; Calculator
  :custom
  (math-additional-units
   '((GiB "1024 * MiB" "Giga Byte")
     (MiB "1024 * KiB" "Mega Byte")
     (KiB "1024 * B" "Kilo Byte")
     (B nil "Byte")
     (Gib "1024 * Mib" "Giga Bit")
     (Mib "1024 * Kib" "Mega Bit")
     (Kib "1024 * b" "Kilo Bit")
     (b "B / 8" "Bit")))
  :config
  (setq math-units-table nil))

(use-package emacs
   :config
   (load-theme 'wombat))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "/usr/bin/markdown"))

;; C-mode for my own and kernel styles
(require 'linux-kernel-c-style)
(defun dr/c-mode-hook ()
    ;; Enable kernel mode for the appropriate files
  (if (and buffer-file-name
           (string-match "linux.git" buffer-file-name))
      (linux-kernel-set-c-style)
    (progn
      (setq c-default-style "linux"
            c-basic-offset 4))))

(require 'dr-auto-insert)

;; Key-bindings
(define-key global-map (kbd "M-g") 'goto-line)
; Let C-x o work across frames if there is only one window
(defun my-other-window-or-frame ()
  "Switch to another frame if only one window exists."
  (interactive)
  (let ((try-other-frames
         (and (> (length (frame-list)) 1)
              (eq (length (window-list)) 1))))
    (if try-other-frames            ; This should be unnecessary
        (other-frame 1)             ; but I'm too lazy to fix it
      (other-window 1))))

(define-key global-map (kbd "C-x o") 'my-other-window-or-frame)
