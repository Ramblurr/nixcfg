;;; $DOOMDIR/+clojure.el -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(use-package! clojure-mode
  :mode "\\.clj$"
  :mode "\\.edn$"
  :mode "\\(?:build\\|profile\\)\\.boot$"
  :mode ("\\.cljs$" . clojurescript-mode)
  :mode ("\\.cljc$" . clojurec-mode)
  :config
  (setq clojure-toplevel-inside-comment-form t)
  (setq clojure-align-forms-automatically t)
  (define-clojure-indent
   (>defn :defn)
   (>defn- :defn)
   (defresolver :defn)
   (defcomponent :defn)
   (defscene :defn)
   (defnc :defn)
   (defnc- :defn)
   (defui :defn)
   (defui- :defn)
   (fn-traced :defn)
   (defn-traced :defn))
  )

(use-package! clj-ns-name
  :after clojure-mode
  :config (clj-ns-name-install))

(use-package! cider
  :after clojure-mode
  :config
  (setq
   cider-download-java-sources t
   cider-default-cljs-repl 'shadow
   cider-save-file-on-load t
   cider-result-overlay-position 'at-point ; results shown right after expression
   cider-show-error-buffer t               ;'only-in-repl
   cider-font-lock-dynamically nil         ; use lsp semantic tokens
   cider-eldoc-display-for-symbol-at-point nil ; use lsp
   cider-prompt-for-symbol nil
   cider-reuse-dead-repls nil
   cider-use-xref nil                   ; use lsp
   ;; minimise the repl buffer activity
   cider-repl-buffer-size-limit 100     ; limit lines shown in REPL buffer
   cider-repl-display-help-banner nil   ; disable help banner
   cider-repl-history-size 10           ; limit command history
   cider-repl-pop-to-buffer-on-connect nil ; REPL buffer shown at starup (nil does not show buffer)
   ;; my repl
   cider-clojure-cli-global-options "-M:repl/dev:dev")
  (set-lookup-handlers! '(cider-mode cider-repl-mode) nil) ; use lsp
  (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil))

(after! clj-refactor
  (setq
   cljr-warn-on-eval nil
   cljr-eagerly-build-asts-on-startup nil
   cljr-favor-prefix-notation nil
   cljr-favor-private-functions nil
   cljr-insert-newline-after-require nil
   cljr-magic-require-namespaces
   '(
     ("async" . "clojure.core.async")
     ("csv" . "clojure.data.csv")
     ("datomic" . "datomic.client.api")
     ("ds" . "donut.system")
     ("edn". "clojure.edn")
     ("enc" . "taoensso.encore")
     ("fs" . "me.raynes.fs")
     ("gen" . "clojure.spec.gen.alpha")
     ("gstring" . "goog.string")
     ("http" . "babashka.http-client.client")
     ("ig" . "integrant.core")
     ("io" . "clojure.java.io")
     ("jdbc" . "clojure.java.jdbc")
     ("json" . "jsonista.core")
     ("fs" "babashka.fs")
     ("cli" "babashka.cli")
     ("p" "babashka.process")
     ("log" . "taoensso.telemere")
     ("md" . "malli.destructure")
     ("medley" . "medley.core")
     ("me" . "malli.error")
     ("m" . "malli.core")
     ("mr" . "malli.registry")
     ("mt" . "malli.transform")
     ("mu" . "malli.util")
     ("s" . "clojure.spec.alpha")
     ("set" . "clojure.set")
     ("sh" . "clojure.java.shell")
     ("sql" . "honeysql.core")
     ("str" . "clojure.string")
     ("tg" . "net.modulolotus.truegrit")
     ("t" . "tick.alpha.api")
     ("tufte" . "taoensso.tufte")
     ("walk" . "clojure.walk")
     ("zip" . "clojure.zip")
     ))
  (setq cljr-clojure-test-declaration "[clojure.test :refer [deftest is testing]]")
  (setq cljr-cljs-clojure-test-declaration cljr-clojure-test-declaration)
  (setq cljr-cljc-clojure-test-declaration cljr-clojure-test-declaration))



(use-package! clojure-essential-ref)
(use-package! clojure-essential-ref-nov
  :init
  (setq clojure-essential-ref-nov-epub-path
        "~/docs/books/Clojure_The_Essential_Reference.epub")
  (setq clojure-essential-ref-default-browse-fn
        #'clojure-essential-ref-nov-browse))
