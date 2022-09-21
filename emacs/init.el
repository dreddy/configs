
(add-to-list 'load-path (concat user-emacs-directory
        (convert-standard-filename "lisp/")))
;; Display
(when (display-graphic-p)
      (scroll-bar-mode -1)            ; Disable the scroll bar
      (tool-bar-mode -1)              ; Disable the tool bar
      (tooltip-mode -1))              ; Disable the tooltips

;; Platform specific settings
(cond ((eq system-type 'windows-nt)
       (setq inhibit-compacting-font-caches t)
       (setq create-lockfiles nil))
      ((eq system-type 'darwin)
       (setq mac-command-modifier 'meta
             mac-option-modifier 'super))
      ) ;; end cond

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)
;; Enable disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region  'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Sane behavior
(show-paren-mode 1)
(blink-cursor-mode -1)
;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Default indentation
(setq-default
  indent-tabs-mode nil
  indicate-empty-lines t
  fill-column 80
  c-basic-offset 4
  tab-width 8
  word-wrap 1)

;; Default Behavior
(setq
 line-number-mode t
 column-number-mode t
 ad-redefinition-action 'accept
 cursor-in-non-selected-windows t
 inhibit-startup-screen t
 initial-scratch-message ";; ready\n\n"
 unibyte-display-via-language-environment t
 mouse-yank-at-point t
 require-final-newline t
 scroll-conservatively 1
 auto-save-default nil
 select-enable-clipboard t
 select-enable-primary t
 kill-ring-max 128
 load-prefer-newer t
 mark-ring-max 128
 scroll-conservatively most-positive-fixnum
 select-enable-clipboard t
 use-package-always-ensure t
 vc-follow-symlinks t
 view-read-only t
 custom-file (concat user-emacs-directory "custom.el")
 ;; backup-inhibited t
 backup-directory-alist '((".*" . "~/.config/emacs/autosaves"))
 auto-save-list-file-prefix "~/.config/emacs/autosaves/"
 auto-save-file-name-transforms '((".*" "~/.config/emacs/autosaves/" t))
 create-lockfiles nil
 ;; gnutls-algorithm-priority "NORMAL:-VERS-TLS1.2"
 )

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Theme
(load-theme 'nord t)

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

(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p
                (format "Folder `%s' not present! Create ?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

;; Set proxy for work machines (EC in pdx,sc,sj and personal machines)
(if (string-match "\\(^dreddy\\|^plx\\|^sc\\|^sj\\)" (system-name))
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10.*\\|134.134.*\\|*.intel.com\\)")
          ("http" . "proxy-dmz.intel.com:912")
          ("https" . "proxy-dmz.intel.com:912")
          ("socks" . "proxy-dmz.intel.com:1080")
          )))

;; Package configuration
(require 'package)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))


(when (version< emacs-version "27.0") (package-initialize))
;; Install dependencies
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package ggtags
    :ensure t
    :commands ggtags-mode
    :config
    (unbind-key "M-<" ggtags-mode-map)
    (unbind-key "M->" ggtags-mode-map))

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize)))

(use-package yasnippet
  :ensure t
  :defer t
  :init
    (yas-global-mode 1))

(use-package magit
  :ensure t
  :defer t
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c G" . magit-dispatch-popup))
  :config
  (setq magit-diff-refine-hunk t)
  (setq global-magit-file-mode 1))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :ensure nil
  :config
  (setq
   python-shell-interpreter "python3"
   python-indent-offset 4
   python-indent-guess-indent-offset-verbose nil)
  (add-hook 'python-mode-hook 'hs-minor-mode))

(use-package toml-mode)

(use-package rust-mode
    :config
  (setq rust-format-on-save t)
  (setq rust-indent-offset 4))

(use-package cargo-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package org
  :ensure nil
  :config
  (setq
   org-log-done t
   org-src-fontify-natively t
   org-startup-indented t
   org-startup-folded nil))


(require 'linux-kernel-c-style)
(defun dr/c-mode-hook ()
    ;; Enable kernel mode for the appropriate files
  (if (and buffer-file-name
           (string-match "linux.git" buffer-file-name))
      (linux-kernel-set-c-style)
    (progn
      (setq c-default-style "linux"
            c-basic-offset 4))))

(add-hook 'c-mode-hook 'dr/c-mode-hook)

(auto-insert-mode)
(setq auto-insert-alist '())
(setq auto-insert-query nil)

(add-to-list 'auto-insert-alist
             '(python-mode
               nil
               "#!/usr/bin/env python3\n"
               "\n"
               _ "\n"
               "\n"
               "if __name__ == '__main__':\n"
               > "\n\n"
               ))
(add-to-list 'auto-insert-alist
             '(("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\)\\'" . "C++ skeleton")
               nil
               "/*" \n
               (file-name-nondirectory (buffer-file-name)) \n
               " */" > \n \n
               "#include <iostream>" \n \n
               "using namespace std;" \n \n
               "int main()" \n
               -4 "{" \n
               > _ \n
               > _ "return 0;" \n
               -4 "}" > \n))

(defvar emacs-start-time)
(add-hook 'emacs-startup-hook #'emacs-startup-message)
(defun emacs-startup-message ()
  "Display a message after Emacs startup."
  (defconst emacs-load-time
    (float-time (time-subtract (current-time) emacs-start-time)))
  (message "Emacs loaded in %.1f seconds."
           emacs-load-time))
