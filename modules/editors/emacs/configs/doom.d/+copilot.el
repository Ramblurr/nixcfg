;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-SPC" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word))
  :config
  (setq copilot-indent-offset-warning-disable t)
  (set-face-foreground 'copilot-overlay-face "pink")
  (customize-set-variable 'copilot-enable-predicates '(evil-insert-state-p))


  (defun my/copilot-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (indent-for-tab-command)))

  (with-eval-after-load 'copilot
    (evil-define-key 'insert copilot-mode-map
      (kbd "<tab>") #'my/copilot-tab))

  ;; Disable Copilot when editing Lisp
  ;; Copilot really sucks in Lisp
  ;; (setq copilot-disable-predicates
  ;;   (list
  ;;     (lambda () (when (derived-mode-p 'clojure-mode 'lisp-mode 'emacs-lisp-mode) t))))
  )
