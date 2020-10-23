;; This configuration needs to work with emacs25/26/27 and with/without ssl - dreddy

(add-to-list 'load-path
             (concat user-emacs-directory (convert-standard-filename "lisp/")))

;; Display
(when (display-graphic-p)
      (scroll-bar-mode -1)            ; Disable the scroll bar
      (tool-bar-mode -1)              ; Disable the tool bar
      (tooltip-mode -1)              ; Disable the tooltips
      (progn
        (setq initial-frame-alist
              '((width . 90) (height . 50) (tool-bar-lines . 0) (inhibit-double-buffering . t)))
        (setq default-frame-alist
              '((width . 90) (height . 50) (tool-bar-lines . 0) (inhibit-double-buffering . t)))
        ))

(load-theme 'my-solarized t)

;; Platform specific settings
(cond ((eq system-type 'windows-nt)
       (setq inhibit-compacting-font-caches t))
      ((eq system-type 'darwin)
       (setenv "PATH" "/usr/local/bin:$PATH" t)
       (setq mac-command-modifier 'meta
             mac-option-modifier 'super))
      ) ;; end cond

;; Default indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              word-wrap 1)

(fset 'yes-or-no-p 'y-or-n-p)

;; Enable disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region  'disabled nil)
(put 'downcase-region  'disabled nil)

;; Default Behavior
(setq
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 cursor-in-non-selected-windows t                 ; Hide the cursor in inactive win
 inhibit-startup-screen t
 initial-scratch-message ";; ready\n\n"
 unibyte-display-via-language-environment t
 backup-inhibited t
 mouse-yank-at-point t
 require-final-newline t
 scroll-conservatively 1
 column-number-mode t
 auto-save-default nil
 select-enable-clipboard t
 select-enable-primary t
 kill-ring-max 128
 load-prefer-newer t
 mark-ring-max 128
 scroll-conservatively most-positive-fixnum
 select-enable-clipboard t
 tab-width 4
 use-package-always-ensure t
 vc-follow-symlinks t
 view-read-only t
 auto-save-list-file-prefix (concat user-emacs-directory (convert-standard-filename "tmp/autosaves"))
 custom-file (concat user-emacs-directory "custom.el")
 )


(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(show-paren-mode 1)
;(global-hl-line-mode )                             ; Hightlight current line
(blink-cursor-mode -1)

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

;; Set proxy for work machines (EC in pdx,sc,sj and personal machines)
(if (string-match "\\(^dreddy\\|^plx\\|^sc\\|^sj\\)" (system-name))
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10.*\\|134.134.*\\|*.intel.com\\)")
          ("http" . "proxy-dmz.intel.com:911")
          ("https" . "proxy-dmz.intel.com:912")
          ("ftp" . "proxy-dmz.intel.com:911")
          )))


;;; Install into separate package dirs for each Emacs version,
;;; to use a single filesystem on multiple environments
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

;; Package configuration
(require 'package)

;; Let's see how long the insecure urls last
(let* ((proto (if (gnutls-available-p) "https" "http")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/"))))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.2")

(when (version< emacs-version "27.0") (package-initialize))
;; Install dependencies
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'my-packages)
