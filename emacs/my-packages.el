(use-package use-package-ensure-system-package
  :ensure t)

(use-package flx
  ;; Provides fuzzy matching for ivy completion
  :ensure t
  :defer t)

(use-package ggtags
    :ensure t
    :commands ggtags-mode
    :config
    (unbind-key "M-<" ggtags-mode-map)
    (unbind-key "M->" ggtags-mode-map))

(use-package ivy
  :ensure t
  :defer t
  :requires flx
  :diminish ivy-mode
  :init (ivy-mode 1)
  :bind (:map ivy-minibuffer-map ("C-m" . ivy-alt-done))
  :config (ivy-mode 1)
	(setq ivy-count-format ""
                ivy-display-style nil
                ivy-minibuffer-faces nil
                ivy-use-virtual-buffers t
                ivy-count-format "%d/%d "
                ivy-re-builders-alist '((t . ivy--regex-fuzzy))))


(use-package yasnippet
  :ensure t
  :defer t
  :init
    (yas-global-mode 1))

; style I want to use in QII c++ mode
(c-add-style "my-style"
             '("stroustrup"
               (indent-tabs-mode . nil)        ; use spaces rather than tabs
               (c-basic-offset . 4)            ; indent by four spaces
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (innamespace . [0])
                                   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)         
  (c-toggle-auto-hungry-state 1))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defun my-c-mode-hook()
  (setq c-basic-offset 4)
  (setq c-default-style "linux")
  (setq indent-tabs-mode nil)
  (ggtags-mode)
  )
(add-hook 'c-mode-hook 'my-c-mode-hook)

(provide 'my-packages)
