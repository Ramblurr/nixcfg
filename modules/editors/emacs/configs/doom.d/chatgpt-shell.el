(use-package! chatgpt-shell
  :config
  (setq chatgpt-shell-openai-key
        (lambda () (auth-source-pick-first-password :host "api.openai.com"))))
