(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" default))
 '(safe-local-variable-values
   '((eval progn
      (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
      (add-to-list 'cider-jack-in-nrepl-middlewares "cider.nrepl/cider-middleware"))
     (cider-clojurec-eval-destination . 'cljs)
     (cider-repl-display-help-banner)
     (cider-redirect-server-output-to-repl . t)
     (cider-preferred-build-tool . clojure-cli)
     (cider-clojure-cli-global-options . "")
     (cider-clojure-cli-parameters . "")
     (cider-shadow-cljs-default-options . "app")
     (cider-clojure-cli-global-options . "-A:dev -A:test"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
