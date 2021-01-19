;;
;; My version of the Solarized color theme for Emacs.
;;
;; Excessively commented because I'm trying to learn Emacs Lisp.
;;

(deftheme my-solarized "Mix of sellout's, bbatsov's and
wasamasa's fork of bbatsov's Solarized theme.")

(defun my-solarized-toggle ()
  (interactive)
  (if my-solarized-dark?
      (setq my-solarized-dark? nil)
    (setq my-solarized-dark? t))
  (load-theme 'my-solarized t))

(defvar my-solarized-dark? t
  "t for dark, nil for light.")

(defun my-solarized-color (color)
  "Maps Solarized color name to hex color. Base colors are adjusted
for light/dark depending on the value of `my-solarized-dark'."
  (cond
   ((eq color 'bg-3) (if my-solarized-dark? "#002b36" "#fdf6e3"))
   ((eq color 'bg-2) (if my-solarized-dark? "#073642" "#eee8d5"))
   ((eq color 'bg-1) (if my-solarized-dark? "#586e75" "#93a1a1"))
   ((eq color 'bg-0) (if my-solarized-dark? "#657b83" "#839496"))
   ((eq color 'fg-0) (if my-solarized-dark? "#839496" "#657b83"))
   ((eq color 'fg-1) (if my-solarized-dark? "#93a1a1" "#586e75"))
   ((eq color 'fg-2) (if my-solarized-dark? "#eee8d5" "#073642"))
   ((eq color 'fg-3) (if my-solarized-dark? "#fdf6e3" "#002b36"))
   ((eq color 'yellow) "#b58900")
   ((eq color 'orange) "#cb4b16")
   ((eq color 'red) "#dc322f")
   ((eq color 'magenta) "#d33682")
   ((eq color 'violet) "#6c71c4")
   ((eq color 'blue) "#268bd2")
   ((eq color 'cyan) "#2aa198")
   ((eq color 'green) "#859900")))

(defun my-apply-faces (faces)
  "Helper function that sets the faces of the theme to a list of FACES.
See `my-transform-face' for the transformation, see
`my-transform-spec' for the rules."
  (apply #'custom-theme-set-faces 'my-solarized
         (mapcar #'my-transform-face faces)))

(defun my-transform-face (face)
  "Helper function that transforms a FACE. FACE is a list where the
first element is the name of the affected face and the remaining
elements specify the face attributes which are transformed into face
attributes for graphical displays. See `my-transform-spec' for the
rules that are applied to the face attributes."
  (let* ((name (car face)) ; car = first
         (spec (cdr face)) ; cdr = rest
	 (graphic-spec (my-transform-spec spec)))
    `(,name ((((type graphic)) ,@graphic-spec)))))

(defun my-transform-spec (spec)
  "Helper function that transforms SPEC. SPEC is a property list where
the values are substituted with colors from `my-solarized-colors' for
keys which are either :foreground or :background. All other key-value
combinations remain unchanged."
  (let (output)
    (while spec
      (let* ((key (car spec))
             (value (cadr spec)) ; cadr = car of the cdr = first of the rest
             (color (my-solarized-color value)))
        (cond
         ((memq key '(:foreground :background :overline :color))
          (setq output (append output (list key color))))
	 ((and (memq key '(:box :underline)) (listp value)) ; recurse if nested
	  (setq output (append output (list key (my-transform-spec value)))))
         (t (setq output (append output (list key value))))))
      (setq spec (cddr spec))) ; cdr of the cdr = rest of the rest
    output))

;;
;; Here goes the actual face definitions
;;
;; For the dark version, this is the mapping of monotones to
;; symbols:
;;
;;   base1   fg-1  Emphasized content
;;   base0   fg-0  Primary content
;;   base01  bg-1  Secondary content
;;   base02  bg-2  Background highlights
;;   base03  bg-3  Background
;;
;; Accent colors are mapped to corresponding symbols.
;;
;; After changing, `M-x eval buffer` to apply changes.
;;
;; Rules:
;;
;; - Links are violet.
;;
(my-apply-faces
 '(
   ;; 
   ;; First some basic ones.
   
   (default :foreground fg-0 :background bg-3)
   (cursor :foreground bg-3 :background bg-0)
   (shadow :foreground bg-1)
   (link :foreground violet :underline t)
   (link-visited :foreground magenta :underline t)
   (match :foreground yellow :inverse-video t) ; search
   (error :foreground red :inverse-video t)
   (warning :foreground red)
   (success :foreground blue)
   (escape-glyph :foreground red)
   (lazy-highlight :inherit match)
   (fringe :foreground bg-1 :background bg-2)
   (highlight :background bg-2)
   (widget-field :foreground fg-1 :background bg-2 :box (:line-width 1 :color fg-2) :inherit default)
   (widget-single-line-field :inherit widget-field)
   (button :underline t)
   
   (header-line :foreground fg-0 :background bg-2 :inverse-video t)
   (menu :foreground fg-0 :background bg-2)
   (minibuffer-prompt :foreground cyan)
   (mode-line :foreground fg-1 :background bg-2 :inverse-video t :box nil)
   (mode-line-inactive :foreground fg-0 :background bg-2 :inverse-video t :box nil) ; FIXME
   (region :foreground bg-1 :background bg-3 :inverse-video t)
   (secondary-selection :background bg-2)
   (trailing-whitespace :background red :inverse-video t)
   (vertical-border :foreground fg-0)

   (isearch :foreground orange :background bg-3 :inverse-video t)
   (isearch-fail :inherit error)

   ;;
   ;; Then organized by mode, in alphanumeric order.
   
   ;; makefile
   (makefile-space :background magenta)

   ;; circe
   (lui-button-face :foreground violet :underline t)
   (lui-highlight-face :foreground fg-2)
   (lui-time-stamp-face :foreground bg-1)

   (circe-fool-face :foreground bg-1)
   (circe-highlight-nick-face :foreground fg-2)
   (circe-highlight-all-nicks-face :foreground blue)
   (circe-my-message-face :foreground fg-1)
   (circe-originator-face :foreground fg-1)
   (circe-prompt-face :foreground magenta :background bg-3)
   (circe-server-face :inherit font-lock-comment-face)
   (circe-topic-diff-new-face :background green)
   (circe-topic-diff-removed-face :background red)

   ;; custom
   (custom-button
    :foreground fg-1 :background bg-2 :box (:line-width 2 :style released-button))
   (custom-button-mouse :inverse-video t :foreground fg-1 :background bg-2 :inherit custom-button)
   (custom-button-pressed :inverse-video t :foreground fg-1 :background bg-2 :box (:line-width 2 :style pressed-button) :inherit custom-button-mouse)
   (custom-changed :inverse-video t :foreground blue :background fg-3)
   (custom-comment :foreground fg-1 :background bg-2)
   (custom-comment-tag :foreground fg-1 :background bg-2)
   (custom-documentation :inherit default)
   (custom-group-tag :foreground fg-1)
   (custom-group-tag-1 :foreground fg-1)
   (custom-invalid :inverse-video t :foreground red :background bg-3)
   (custom-link :foreground violet)
   (custom-state :foreground green)
   (custom-variable-tag :foreground fg-1)

   ;; diff
   (diff-added :foreground green :background bg-3)
   (diff-changed :foreground blue :background bg-3)
   (diff-removed :foreground red :background bg-3)
   (diff-header :background bg-3)
   (diff-file-header :background bg-3 :foreground fg-0 :weight bold)
   (diff-refine-added :foreground bg-3 :background green)
   (diff-refine-change :foreground bg-3 :background blue)
   (diff-refine-removed :foreground bg-3 :background red)

   ;; dired
   (dired-directory :foreground blue :weight normal)
   (dired-flagged :foreground red)
   (dired-header :foreground bg-3 :background blue)
   (dired-ignored :inherit shadow)
   (dired-mark :foreground yellow)
   (dired-marked :foreground magenta)
   (dired-perm-write :foreground fg-0 :underline t)
   (dired-symlink :foreground cyan :weight normal :slant italic)
   (dired-warning :foreground orange :underline t)

   ;; font-lock
   (font-lock-builtin-face :foreground green) ; Statement
   (font-lock-comment-face :slant italic :foreground bg-1)
   (font-lock-constant-face :foreground cyan)
   (font-lock-function-name-face :foreground blue) ; Identifier
   (font-lock-keyword-face :foreground green) ; Statement
   (font-lock-string-face :foreground cyan) ; Constant
   (font-lock-type-face :foreground yellow)
   (font-lock-variable-name-face :foreground blue) ; Identifier
   (font-lock-warning-face :foreground red)
   (font-lock-doc-face :slant italic :foreground bg-1) ; Comment
   (font-lock-comment-delimiter-face :slant italic :foreground bg-1) ; Comment
   (font-lock-preprocessor-face :foreground orange) ; PreProc
   (font-lock-negation-char-face :foreground red)
   (font-lock-regexp-grouping-construct :foreground orange)
   (font-lock-regexp-grouping-backslash :foreground yellow)

   (font-lock-other-emphasized-face :slant italic :foreground violet)
   (font-lock-color-constant-face :foreground green)
   (font-lock-special-keyword-face :foreground red) ; Special
   (font-lock-other-type-face :slant italic :foreground blue)
   (font-lock-exit-face :foreground red)
   (font-lock-reference-face :foreground cyan)
   
   ;; ;; flx
   ;; (flx-highlight-face :foreground blue :weight normal :underline nil)

   ;; grep
   (grep-context-face :foreground fg-0)
   (grep-error-face :foreground red :underline t)
   (grep-hit-face :foreground blue)
   (grep-match-face :foreground orange)

   ;; hl-line-mode
   (hl-line :background bg-2 :underline nil)

   ;; ;; highlight-indentation
   (highlight-indentation-face :background bg-2)
   (highlight-indentation-current-column-face :background bg-2)

   ;; ido
   (ido-first-match :foreground green)
   (ido-only-match :foreground green)
   (ido-subdir :foreground blue)

   ;; linum
   (linum :foreground bg-1 :background bg-2)
   
   ;; magit
   ;; (magit-section-title :foreground yellow :weight bold)
   ;; (magit-branch :foreground orange :weight bold)
   ;; (magit-diff-add :background unspecified :foreground green)
   ;; (magit-diff-del :background unspecified :foreground red)
   ;; (magit-item-highlight :background bg-2 :weight unspecified)
   ;; (magit-log-author :foreground cyan)
   ;; (magit-log-graph :foreground bg-1)
   ;; 					;   (magit-log-head-label-bisect-bad :background red-l :foreground red-d :box (:line-width -1))
   ;; 					;   (magit-log-head-label-bisect-good :background green-l :foreground green-d :box (:line-width -1))
   ;; (magit-log-head-label-default :background bg-2 :box (:line-width -1))
   ;; 					;   (magit-log-head-label-local :background blue-d :foreground blue-l :box (:line-width -1))
   ;; 					;   (magit-log-head-label-patches :background red-d :foreground red-l :box (:line-width -1))
   ;; 					;   (magit-log-head-label-remote :background green-d :foreground green-l :box (:line-width -1))
   ;; 					;   (magit-log-head-label-tags :background yellow-d :foreground yellow-l :box (:line-width -1))
   ;; (magit-log-sha1 :foreground yellow)

   ;; markdown-mode
   (markdown-blockquote-face :inherit font-lock-doc-face)
   (markdown-bold-face :inherit bold)
   (markdown-comment-face :inherit font-lock-comment-face)
   (markdown-footnote-face :inherit shadow)
   (markdown-header-delimiter-face :inherit shadow)
   (markdown-header-face :inherit outline-1)
   (markdown-header-face-1 :inherit outline-1)
   (markdown-header-face-2 :inherit outline-2)
   (markdown-header-face-3 :inherit outline-3)
   (markdown-header-face-4 :inherit outline-4)
   (markdown-header-face-5 :inherit outline-5)
   (markdown-header-face-6 :inherit outline-6)
   (markdown-header-rule-face :inherit shadow)
   (markdown-inline-code-face :foreground bg-1)
   (markdown-italic-face :inherit italic)
   (markdown-list-face :foreground bg-1)
   (markdown-link-face :inherit shadow)
   (markdown-link-title-face :inherit shadow)
   (markdown-missing-link-face :inherit font-lock-warning-face)
   (markdown-pre-face :foreground bg-1)
   (markdown-reference-face :inherit shadow)
   (markdown-url-face :inherit shadow)
   (markdown-language-keyword-face :inherit font-lock-keyword-face)
   
   ;; org-mode
   (org-block-background :background bg-2)
   (org-hide :foreground bg-3)
   (org-todo :foreground bg-3 :background red)
   (org-done :foreground green)
   (org-todo-kwd-face :foreground bg-3 :background red)
   (org-done-kwd-face :foreground green)
   (org-project-kwd-face :foreground violet :background bg-3)
   (org-waiting-kwd-face :foreground orange :background bg-3)
   (org-someday-kwd-face :foreground blue :background bg-3)
   (org-started-kwd-face :foreground yellow :background bg-3)
   (org-cancelled-kwd-face :foreground green :background bg-3)
   (org-delegated-kwd-face :foreground cyan :background bg-3)
   (org-default :inherit default)
   (org-level-1 :inherit outline-1)
   (org-level-2 :inherit outline-2)
   (org-level-3 :inherit outline-3)
   (org-level-4 :inherit outline-4)
   (org-level-5 :inherit outline-5)
   (org-level-6 :inherit outline-6)
   (org-level-7 :inherit outline-7)
   (org-level-8 :inherit outline-8)
   (org-special-keyword :slant italic :foreground bg-1)
   (org-drawer :foreground blue)
   (org-column :inverse-video t :foreground cyan)
   (org-column-title :weight bold :inverse-video t)
   (org-warning :foreground red)
   (org-archived :foreground bg-1)
   (org-link :underline t :foreground violet)
   (org-footnote :underline t :foreground violet)
   (org-ellipses :foreground yellow :strike-through t)
   (org-target :underline t)
   (org-date :underline t :foreground violet)
   (org-date-selected :inverse-video t :foreground red)
   (org-sexp-date :underline t :foreground violet)
   (org-tag :weight bold)
   (org-list-dt :weight bold)
   (org-agenda-done :foreground green)
   (org-headline-done :foreground bg-1)
   (org-priority :slant italic :foreground bg-1)
   (org-checkbox :weight bold)
   (org-table :foreground cyan)
   (org-formula :weight bold :slant italic :foreground red)
   (org-code :foreground bg-1)
   (org-document-title :weight bold :foreground cyan)
   (org-document-info-keyword :foreground bg-1)
   (org-block :foreground bg-1)
   (org-verbatim :underline t :foreground bg-1)
   (org-clock-overlay :inverse-video t :background cyan :foreground bg-3)
   (org-agenda-structure :weight bold :foreground blue)
   (org-scheduled :weight bold :slant italic :foreground green)
   (org-scheduled-today :weight bold :slant italic :foreground green)
   (org-agenda-dimmed-todo-face :foreground bg-1)
   (org-scheduled-previously :weight bold :foreground red)
   (org-upcoming-deadline :weight bold :foreground red)
   (org-agenda-restriction-lock :inverse-video t :foreground bg-3 :background cyan)
   (org-time-grid :foreground yellow)
   (org-latex-and-related:foreground orange)

   ;; outline
   (outline-1 :foreground yellow)
   (outline-2 :foreground cyan)
   (outline-3 :foreground blue)
   (outline-4 :foreground violet)
   (outline-5 :foreground fg-0)
   (outline-6 :foreground bg-1)
   (outline-7 :foreground bg-1)
   (outline-8 :foreground bg-1)

   ;; sh-mode
   (sh-quoted-exec :foreground violet)
   (sh-escaped-newline :foreground yellow)
   (sh-heredoc :foreground yellow)

   ;; table
   (table-cell :foreground fg-0 :background bg-3)

   ;; term
   (term-color-black :foreground bg-2)
   (term-color-red :foreground red)
   (term-color-green :foreground green)
   (term-color-yellow :foreground yellow)
   (term-color-blue :foreground blue)
   (term-color-magenta :foreground magenta)
   (term-color-cyan :foreground cyan)
   (term-color-white :foreground fg-0)
   (term-default-fg-color :inherit term-color-white)
   (term-default-bg-color :inherit term-color-black)

   ;; web-mode
   (web-mode-builtin-face :foreground red)
   (web-mode-comment-face :foreground bg-1)
   (web-mode-constant-face :foreground blue)
   (web-mode-current-element-highlight-face :underline unspecified :weight unspecified :background bg-2)
   (web-mode-css-at-rule-face :foreground violet :slant italic)
   (web-mode-css-pseudo-class-face :foreground green :slant italic)
   (web-mode-doctype-face :foreground bg-1 :slant italic :weight bold)
   (web-mode-folded-face :underline t)
   (web-mode-function-name-face :foreground blue)
   (web-mode-html-attr-name-face :foreground blue :slant normal)
   (web-mode-html-attr-value-face :foreground cyan :slant italic)
   (web-mode-html-tag-face :foreground green)
   (web-mode-keyword-face :foreground yellow :weight normal)
   (web-mode-preprocessor-face :foreground yellow :slant normal :weight unspecified)
   (web-mode-string-face :foreground cyan)
   (web-mode-type-face :foreground yellow)
   (web-mode-variable-name-face :foreground blue)
   (web-mode-warning-face :inherit font-lock-warning-face)
   (web-mode-block-attr-name-face :inherit web-mode-html-attr-name-face)
   (web-mode-block-attr-value-face :inherit web-mode-html-attr-value-face)
   (web-mode-block-comment-face :inherit web-mode-comment-face)
   (web-mode-block-control-face :inherit font-lock-preprocessor-face)
   (web-mode-block-face :background unspecified)
   (web-mode-block-string-face :inherit web-mode-string-face)
   (web-mode-comment-keyword-face :box (:line-width -1) :weight bold)
   (web-mode-css-color-face :inherit font-lock-builtin-face)
   (web-mode-css-function-face :inherit font-lock-builtin-face)
   (web-mode-css-priority-face :inherit font-lock-builtin-face)
   (web-mode-css-property-name-face :inherit font-lock-variable-name-face)
   (web-mode-css-selector-face :inherit font-lock-keyword-face)
   (web-mode-css-string-face :inherit web-mode-string-face)
   (web-mode-javascript-string-face :inherit web-mode-string-face)
   (web-mode-json-context-face :foreground violet)
   (web-mode-json-key-face :foreground violet)
   (web-mode-json-string-face :inherit web-mode-string-face)
   (web-mode-param-name-face :foreground fg-0)
   (web-mode-part-comment-face :inherit web-mode-comment-face)
   (web-mode-part-face :inherit web-mode-block-face)
   (web-mode-part-string-face :inherit web-mode-string-face)
   (web-mode-symbol-face :foreground yellow)
   (web-mode-whitespace-face :background red)

   ;; whitespace-mode
   (whitespace-space :background unspecified :foreground bg-1 :inverse-video unspecified :slant italic)
   (whitespace-hspace :background unspecified :foreground fg-1 :inverse-video unspecified)
   (whitespace-tab :background unspecified :foreground red :inverse-video unspecified :weight bold)
   (whitespace-newline :background unspecified :foreground bg-1 :inverse-video unspecified)
   (whitespace-trailing :background unspecified :foreground orange :inverse-video t)
   (whitespace-line :background unspecified :foreground magenta :inverse-video unspecified)
   (whitespace-space-before-tab :background red :foreground unspecified :inverse-video unspecified)
   (whitespace-indentation :background unspecified :foreground yellow :inverse-video unspecified)
   (whitespace-empty :background unspecified :foreground red :inverse-video t)
   (whitespace-space-after-tab :background unspecified :foreground orange :inverse-video t)
   ))

(custom-theme-set-variables
 'my-solarized
 ;; ansi-term
 `(ansi-color-names-vector [,(my-solarized-color 'bg-3)
                            ,(my-solarized-color 'red)
                            ,(my-solarized-color 'green)
                            ,(my-solarized-color 'yellow)
                            ,(my-solarized-color 'blue)
                            ,(my-solarized-color 'magenta)
                            ,(my-solarized-color 'cyan)
                            ,(my-solarized-color 'fg-0)])

 ;; compilation
 '(compilation-message-face 'default)

 ;; magit
 '(magit-use-overlays nil)

 ;; org-mode
 `(org-todo-keyword-faces
   '(("PDNG" . (:background ,(my-solarized-color 'bg-3) :foreground ,(my-solarized-color 'orange) :slant italic))
     ("TODO" . (:background ,(my-solarized-color 'bg-3) :foreground ,(my-solarized-color 'red))))))

(provide-theme 'my-solarized)
