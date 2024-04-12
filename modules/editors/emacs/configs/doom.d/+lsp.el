;;; +lsp.el -*- lexical-binding: t; -*-



;; Tell the LSP to not monitor vendor dirs
(after! lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor\\'" t))

(use-package! lsp-mode
  :commands lsp
  :config
  ;; Core
  (setq
   ;;   ;; https://emacs-lsp.github.io/lsp-mode/page/settings/headerline/
   ;;   lsp-headerline-breadcrumb-enable t           ; Breadcrumb trail
   ;;   lsp-headerline-breadcrumb-segments '(symbols) ; namespace & symbols, no file path

   ;;   lsp-signature-render-documentation nil
   ;;   lsp-signature-function 'lsp-signature-posframe
   lsp-semantic-tokens-enable t
   ;;   lsp-idle-delay 0.2 ;; Smoother LSP features response in cost of performance (Most servers I use have good performance)
   lsp-use-plists nil)
  ;; (add-hook 'lsp-after-apply-edits-hook (lambda (&rest _) (save-buffer)))
  ;; (add-hook 'lsp-mode-hook (lambda () (setq-local company-format-margin-function #'company-vscode-dark-icons-margin)))

  (after! nix-mode
    (setq! lsp-nix-nil-formatter ["nixfmt" "-w" "100"])))

(use-package! lsp-treemacs
  :config
  (setq lsp-treemacs-error-list-current-project-only t))

;; (use-package! lsp-ui
;;   :after lsp-mode
;;   :commands lsp-ui-mode
;;   :config
;;   (setq lsp-ui-doc-enable nil       ;; disable all doc popups
;;         lsp-ui-sideline-enable nil  ;; disable sideline bar for less distraction
;;         treemacs-space-between-root-nodes nil  ;; no spacing in treemacs views
;;         lsp-log-io t  ; Log client-server json communication
;;         lsp-ui-peek-enable t))
