;;; -*- lexical-binding: t; -*-
;;*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Clojure functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; evaluate some code in a CLJ repl regardless of what type of buffer you're in
(defun my/eval-clojure-code-sync (code)
  (cider-nrepl-sync-request:eval
   code
   (cider-current-repl 'clj 'ensure)
   (cider-current-ns)))

;; Rich Comment Form evaluator
;; Evaluates the *last* form in a clojure buffer that has the ';; rcf' before it.
(defun my/eval-rcf ()
  (interactive) ;; you have to do this to be able to bind it to a key or M-x it
  (save-excursion      ;; this is what returns the point to where you started
    (goto-char (point-max))              ;; go to end of buffer
    (if (re-search-backward " *;; rcf" nil t) ;; search backward, don't throw an error
        (cider-eval-last-sexp) ;; the cider fn which evals the last form
      (message "No rcf found!")))) ;; if the search fails, show a message in the echo area


;; Dev reset - reload
(defun my/clojure-dev-reset ()
  (interactive)
  (cider-interactive-eval
   ;; (format "(ol.app.dev.dev-extras/reset)"
   ;;         (cider-last-sexp))

   (format "(dev/reset)"
           (cider-last-sexp))))

;; Refresh browser command (for non-cljs projects like when using htmx)
(defun my/clojure-reload-browser ()
  "Reload the browser on eval"
  (interactive)
  (cider-interactive-eval
   (format "(browser/refresh)" (cider-last-sexp))))


;; Like my/clojure-reload-browser expect evals the defun first
(defun my/eval-defun-and-reload-browser ()
  "Reload the browser on eval"
  (interactive)
  (cider-eval-defun-at-point)
  (my/clojure-reload-browser))

(defun my/clojure-clean-and-sort-ns ()
  "Clean and sort the clojure active namespace"
  (interactive)
  (lsp-clojure-clean-ns)
  (clojure-sort-ns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integration with portal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun portal.api/clear ()
  (interactive)
  (my/eval-clojure-code-sync "(portal.api/clear)")
  ;; (cider-nrepl-sync-request:eval "#?(:clj (portal.api/clear))")
  ;; (cider-nrepl-sync-request:eval "#?(:cljs (portal.web/clear))")
  ;; (cider-nrepl-sync-request:eval "(#?(:clj portal.api/clear :cljs portal.web/clear))")
  )

(defun portal/invoke-portal-command (command-str)
  (cider-nrepl-sync-request:eval
   (concat "(#?(:clj portal.api/eval-str :cljs portal.web/eval-str) \"" command-str "\")")))

(defun portal.ui.commands/select-root ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-root portal.ui.state/state)"))

(defun portal.ui.commands/select-next ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-next portal.ui.state/state)"))

(defun portal.ui.commands/select-prev ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-prev portal.ui.state/state)"))

(defun portal.ui.commands/select-parent ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-parent portal.ui.state/state)"))

(defun portal.ui.commands/select-child ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-child portal.ui.state/state)"))

(defun portal.ui.commands/history-back ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/history-back portal.ui.state/state)"))

(defun portal.ui.commands/focus-selected ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/focus-selected portal.ui.state/state)"))

(defun portal.ui.commands/set-tree-viewer ()
  (interactive)
  (portal/invoke-portal-command
   "(require '[portal.ui.state :as s])

    (defn set-viewer! [viewer]
      (s/dispatch!
       s/state
       assoc-in
       [:selected-viewers
        (s/get-location
         (s/get-selected-context @s/state))]
       viewer))

    (set-viewer! :portal.viewer/tree)"))


(map!
 :after clojure-mode
 :map (clojure-mode-map clojurescript-mode-map clojurec-mode-map)
 (:localleader
  :desc "Reload REPL" :n "R"  #'my/clojure-dev-reset
  (:prefix ("x" . "Portal Inspector")
   :desc "Select Root"    :n "r"   #'portal.ui.commands/select-root
   :desc "Select next"    :n "j"   #'portal.ui.commands/select-next
   :desc "Select prev"    :n "k"   #'portal.ui.commands/select-prev
   :desc "Select parent"  :n "h"   #'portal.ui.commands/select-parent
   :desc "Select child"   :n "l"   #'portal.ui.commands/select-child
   :desc "History back"   :n "C-h" #'portal.ui.commands/history-back
   :desc "Focus selected" :n "RET" #'portal.ui.commands/focus-selected
   :desc "Clear"          :n "x"   #'portal.api/clear)
  (:prefix ("e" . "eval")
   "D" nil
   :desc "Eval defn and reload browser" :n"D" 'my/eval-defun-and-reload-browser
   :desc "Evaluate RCF" :n "c" 'my/eval-rcf
   ;; "e" 'cider-tap-last-sexp
   ;; "E" 'cider-eval-last-sexp
   )
  (:prefix ("n" . "namespace")
   :desc "Clean and Sort" :n "s"  #'my/clojure-clean-and-sort-ns
   )))

(map!
 :leader
 (:prefix ("k" . "Structural Editing")
          "dx" #'kill-sexp
          "dX" #'backward-kill-sexp
          "c" #'lispy-clone
          "p" #'cljr-raise
          "D" #'cljr-destructure-keys
          ))

(after! cider-mode
  (setq cider-show-error-buffer t                        ; show stacktrace buffer
        cider-print-fn 'puget                            ; pretty printing with sorted keys / set values
        ;; cider-enrich-classpath t                         ; disabled until fixed https://github.com/clojure-emacs/cider/issues/3651
        cider-result-overlay-position 'at-point          ; results shown right after expression
        cider-overlays-use-font-lock t

        ;; LSP features over Cider features
        cider-font-lock-dynamically nil                  ; use lsp semantic tokens
        cider-eldoc-display-for-symbol-at-point nil      ; use lsp
        cider-use-xref nil                               ; cider xref to find definitions

        ;; minimise the repl buffer activity
        cider-repl-buffer-size-limit 100                 ; limit lines shown in REPL buffer
        cider-repl-display-help-banner nil               ; disable help banner
        cider-repl-history-size 10                       ; limit command history
        cider-repl-history-file nil                      ; write repl buffer commands to file DOOMDIR/.local/cider-repl-history
        cider-repl-history-highlight-current-entry nil   ; cider default
        cider-repl-history-highlight-inserted-item nil   ; cider default
        cider-repl-history-quit-action 'quit-window      ; restores previous emacs window config (cider default )
        cider-repl-pop-to-buffer-on-connect nil          ; REPL buffer shown at starup (nil does not show buffer)
        cider-repl-use-clojure-font-lock nil
        cider-repl-use-pretty-printing nil
        cider-repl-wrap-history t)
  (set-lookup-handlers! '(cider-mode cider-repl-mode) nil) ; use lsp
  (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil)
  ;; use lsp completion
  (add-hook 'cider-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'cider-complete-at-point)))
  ;; This is needed to send the result of cider evaluate to portal
  ;; (defun cider-tap (&rest r)
  ;;   (cons (concat "(let [__value "
  ;;           (caar r)
  ;;           "] (tap> __value) __value)")
  ;;     (cdar r)))

  ;; (advice-add 'cider-nrepl-request:eval
  ;;             :filter-args #'cider-tap)
  )

(after! clojure-mode
  (setq clojure-indent-style 'align-arguments)
  ;; evaluate expressions in comment as top level
  (setq clojure-toplevel-inside-comment-form t)
  (setq cider-save-file-on-load t)
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
  (setq cider-default-cljs-repl 'shadow)
  (setq clojure-align-forms-automatically t)
  (put '>defn 'clojure-doc-string-elt 2)
  (put '>defn- 'clojure-doc-string-elt 2)
  (put 'defsys 'clojure-doc-string-elt 2)
  (put 'defevent-db 'clojure-doc-string-elt 2)
  (put 'defevent-fx 'clojure-doc-string-elt 2)
  (put 'defsub 'clojure-doc-string-elt 2)
  (put 'defhandler 'clojure-doc-string-elt 2)
  (put 'defstream 'clojure-doc-string-elt 2)
  (put 'defn-traced 'clojure-doc-string-elt 2)
  (put 'defui 'clojure-doc-string-elt 2)
  (put 'defui- 'clojure-doc-string-elt 2)
  (put 'defnc 'clojure-doc-string-elt 2)
  (put 'defnc- 'clojure-doc-string-elt 2)
  (put 'defresolver 'clojure-doc-string-elt 2)
  (put 'defscene 'clojure-doc-string-elt 2)


  (setq cljr-magic-require-namespaces
        '(("io" . "clojure.java.io")
          ("sh" . "clojure.java.shell")
          ("jdbc" . "clojure.java.jdbc")
          ("set" . "clojure.set")
          ("str" . "clojure.string")
          ("gstring" . "goog.string")
          ("time" . "java-time")
          ("path" . "pathetic.core")
          ("walk" . "clojure.walk")
          ("zip" . "clojure.zip")
          ("async" . "clojure.core.async")
          ("component" . "com.stuartsierra.component")
          ("http" . "clj-http.client")
          ("url" . "cemerick.url")
          ("sql" . "honeysql.core")
          ("csv" . "clojure.data.csv")
          ("json" . "jsonista.core")
          ("s" . "clojure.spec.alpha")
          ("fs" . "me.raynes.fs")
          ("ig" . "integrant.core")
          ("cp" . "com.climate.claypoole")
          ("re-frame" . "re-frame.core")
          ("rf" . "re-frame.core")
          ("rf.db" . "re-frame.db")
          ("re" . "reagent.core")
          ("reagent" . "reagent.core")
          ("gen" . "clojure.spec.gen.alpha")
          ("log" . "taoensso.timbre")
          ("enc" . "taoensso.encore")
          ("t" . "tick.alpha.api")
          ("d" . "datahike.api")
          ("dc" . "datahike.core")
          ("p" . "com.wsscode.pathom.core")
          ("pc" . "com.wsscode.pathom.connect")
          ("uism" . "com.fulcrologic.fulcro.ui-state-machines")
          ("df" . "com.fulcrologic.fulcro.data-fetch")
          ("dr" . "com.fulcrologic.fulcro.routing.dynamic-routing")
          ("dom" . "com.fulcrologic.fulcro.dom")
          ("ent" . "com.fulcrologic.fulcro.dom.html-entities")
          ("evt" . "com.fulcrologic.fulcro.dom.events")
          ("comp" . "com.fulcrologic.fulcro.components")
          ("mu" . "com.fulcrologic.fulcro.mutations")
          ("merge" . "com.fulcrologic.fulcro.algorithms.merge")
          ("fs" . "com.fulcrologic.fulcro.algorithms.form-state")
          ("m" . "medley.core")))

  ;; NOTE: You do need to have portal on the class path and the easiest way I know
  ;; how is via a clj user or project alias.
  ;; (setq cider-clojure-cli-global-options "-A:portal")
  )

(after! lsp-clojure
  (setq lsp-lens-enable t))

;; use ns rather than file name for clj buffer name
(use-package! clj-ns-name
  :after clojure-mode
  :config
  (clj-ns-name-install))

(put 'cider-clojurec-eval-destination 'safe-local-variable (lambda (_) t))
(put 'cider-clojure-cli-global-options 'safe-local-variable (lambda (_) t))
(put 'cider-shadow-cljs-default-options 'safe-local-variable (lambda (_) t))
