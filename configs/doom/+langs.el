;;; $DOOMDIR/+langs.el -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://www.reddit.com/r/emacs/comments/hw0cqq/does_anyone_have_issues_with_lspuidoc/
(defvar lva--lsp-ui-doc-atpoint-h 10
  "lsp-ui-doc-max-height when lsp-ui is shown at point")
(defvar lva--lsp-ui-doc-atpoint-w 50
  "lsp-ui-doc-max-width when lsp-ui is shown at point")
(defvar lva--lsp-ui-doc-anchored-h 20
  "lsp-ui-doc-max-height when lsp-ui position is top or bottom")
(defvar lva--lsp-ui-doc-anchored-w 150
  "lsp-ui-doc-max-width when lsp-ui position is top or bottom")
(defun lva/lsp-ui-toggle-doc (arg)
  (interactive "P")
  (if lsp-ui-doc-mode
      (lsp-ui-doc-mode 0)
    (progn
      (if arg
          (setq lsp-ui-doc-position 'at-point
                lsp-ui-doc-max-height lva--lsp-ui-doc-atpoint-h
                lsp-ui-doc-max-width lva--lsp-ui-doc-atpoint-w)
        (setq lsp-ui-doc-position 'top
              lsp-ui-doc-max-height lva--lsp-ui-doc-anchored-h
              lsp-ui-doc-max-width lva--lsp-ui-doc-anchored-w))
      (lsp-ui-doc-mode))))

(use-package! lsp-mode
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable t
        lsp-signature-render-documentation nil
        lsp-signature-function 'lsp-signature-posframe
        lsp-signature-auto-activate nil
        lsp-semantic-tokens-enable t
        lsp-enable-indentation nil
        lsp-inlay-hint-enable t
        lsp-idle-delay  0.05
        lsp-diagnostics-provider :auto
        lsp-use-plists t
        read-process-output-max (* 1024 1024)
        lsp-enable-file-watchers t
        lsp-inline-completion-idle-delay 1
        lsp-copilot-enabled t
        ;; lsp-restart 'auto-restart
        ;; lsp-headerline-breadcrumb-enable t              ; Breadcrumb trail
        ;; lsp-headerline-breadcrumb-segments '(symbols)   ; namespace & symbols, no file path
        ;; lsp-ui-peek-enable nil                          ; popups for refs, errors, symbols, etc.
        ;; lsp-semantic-tokens-enable t                    ; enhance syntax highlight
        ;; lsp-eldoc-enable-hover nil                      ; disable all hover actions
        ;; lsp-ui-doc-enable nil                           ; doc hover popups
        ;; lsp-ui-sideline-enable nil                      ; sidebar code actions visual indicator
        )
  (add-hook 'lsp-after-apply-edits-hook (lambda (&rest _) (save-buffer)))

  (custom-set-faces
   '(lsp-face-highlight-textual ((t (:background "#32302f" :bold t :foreground unspecified ))))
   '(lsp-face-highlight-read ((t (:background "#32302f" :bold t  :foreground unspecified ))))
   '(lsp-face-highlight-write ((t (:background "#32302f" :bold t  :foreground unspecified )))))

  (dolist (dir '("[/\\\\]\\.ccls-cache\\'"
                 "[/\\\\]\\.mypy_cache\\'"
                 "[/\\\\]\\.pytest_cache\\'"
                 "[/\\\\]\\.cache\\'"
                 "[/\\\\]\\.clwb\\'"
                 "[/\\\\]\\.env\\'"
                 "[/\\\\]__pycache__\\'"
                 "[/\\\\]extra\\'"
                 "[/\\\\]dist\\'"
                 "[/\\\\]target\\'"
                 "[/\\\\]out\\'"
                 "[/\\\\]build\\'"
                 "[/\\\\]tmp\\'"
                 "[/\\\\]node_modules\\'"
                 ))
    (push dir lsp-file-watch-ignored-directories))


  (comment
   (let ((clojure-lsp-dev (expand-file-name "/etc/profiles/per-user/ramblurr/bin/clojure-lsp")))
     (when (file-exists-p clojure-lsp-dev)
       (setq  lsp-clojure-custom-server-command `("bash" "-c" ,clojure-lsp-dev))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! lsp-java
  :after java-mode
  :config
  (setq lsp-java-references-code-lens-enabled t
        lsp-java-implementations-code-lens-enabled t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq typescript-indent-level 2)

(comment
 (use-package! prettier-js
   :config
   (add-hook 'js2-mode-hook #'prettier-js-mode)
   (add-hook 'json-mode-hook #'prettier-js-mode)
   (add-hook 'rjsx-mode-hook 'prettier-js-mode)
   (add-hook 'css-mode-hook #'prettier-js-mode)
   (add-hook 'typescript-mode-hook #'prettier-js-mode)
   (add-hook 'typescript-tsx-mode #'prettier-js-mode))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Janet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! ajrepl
  :after janet-ts-mode
  :config
  (add-hook 'janet-ts-mode-hook
            #'ajrepl-interaction-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KDL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! kdl-ts-mode
  :mode "\\.kdl\\'"
  :init)
