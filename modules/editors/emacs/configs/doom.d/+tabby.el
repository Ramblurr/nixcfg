;; accept completion from tabby and fallback to company
(use-package! tabby
  :hook (prog-mode . tabby-mode)
  :bind (:map tabby-mode-map
              ("C-SPC" . 'tabby-accept-completion)
              ("C-TAB" . 'tabby-accept-completion-by-word))
  :config

  (defun my/tabby-tab ()
    (interactive)
    (or (tabby-accept-completion)
        (indent-for-tab-command)))

  (with-eval-after-load 'tabby
    (evil-define-key 'insert tabby-mode-map
      (kbd "<tab>") #'my/tabby-tab))
  )
