;;; -*- lexical-binding: t; -*-
;;*
(map!
 ;; Window Movements
 "C-h"    #'evil-window-left
 "C-j"    #'evil-window-down
 "C-k"    #'evil-window-up
 "C-l"    #'evil-window-right
 "A-q"    #'delete-window
 "C-`"      #'+popup/toggle
 "<C-tab>"  #'+popup/other

 ;; Text-scaling
 "M-+"    (λ! (text-scale-set 0))
 "M-="    #'text-scale-increase
 "M--"    #'text-scale-decrease

 "M-<return>" #'doom/toggle-fullscreen


 ;; Text Editing
 :nv  "gc"   #'evil-commentary

 (:map evil-window-map                  ; prefix "C-w"
  ;; Navigation
  "C-h"     #'evil-window-left
  "C-j"     #'evil-window-down
  "C-k"     #'evil-window-up
  "C-l"     #'evil-window-right
  "C-w"     #'ace-window
  ;; Swapping windows
  "H"       #'+evil/window-move-left
  "J"       #'+evil/window-move-down
  "K"       #'+evil/window-move-up
  "L"       #'+evil/window-move-right
  "C-S-w"   #'ace-swap-window
  ;; Window undo/redo
                                        ;"u"       #'winner-undo
                                        ;"C-u"     #'winner-undo
                                        ;"C-r"     #'winner-redo
  "o"       #'doom/window-enlargen
  ;; Delete window
  "c"       #'+workspace/close-window-or-workspace
  "C-C"     #'ace-delete-window)

 (:after cider-mode
  (:map cider-mode-map
   :leader
   :desc "Lookup documentation at point" :n "d" #'cider-doc
   :desc "Jump to definition at point" :n "l" #'cider-find-var
   :localleader
   ;; :n "b" #'cider-eval-buffer
   ;; :n "B" #'cider-switch-to-repl-buffer
   ;; :n "n" #'cider-repl-set-ns
   ;; :n "j" #'cider-find-var
   ;; :n "s" #'cider-browse-spec
   ;; :n "S" #'helm-cider-spec-ns
   ;; :n "l" #'cljr-move-to-let
   ;; :n "L" #'cljr-introduce-let
   ;; (:desc "docs" :prefix "d"
   ;;   :desc "Browse Namespace" :n "n" #'cider-browse-ns
   ;;   :desc "Browse Spec" :n "s" #'cider-browse-spec
   ;;   :desc "Load ClojureDoc" :n "d" #'cider-clojuredocs)
   ;; :n "h" #'cider-doc
   ;; :n "c" #'cider-repl-clear-buffer
   ;; :n "i" #'cider-inspect-last-result
   ;; :n "p" #'cider-eval-sexp-at-point
   ;; :n "f" #'cider-eval-defun-at-point
   ;; :n "t" #'cider-test-run-ns-tests
   ;; :n "T" #'cider-test-run-test
   )
  ;; (:after cider-browse-ns-mode
  ;;   (:map cider-browse-ns-mode-map
  ;;     :n "RET"       #'cider-browse-ns-operate-at-point))
  )


 ;; Highjacks space/backspace to:
 ;;   a) balance spaces inside brackets/parentheses ( | ) -> (|)
 ;;   b) delete space-indented blocks intelligently
 ;;   c) do none of this when inside a string
 ;; :i [remap newline] #'newline-and-indent


 )

(map!
 :after lispyville
 :map lispyville-mode-map
 :n "M-L" #'lispyville-beginning-of-next-defun
 ;; :n "ge" #'lispyville-backward-atom-end
 ;; :n "C-u"     #'lispyville-delete-back-to-indentation
 )

(map!
  :after clojure-mode
  :n "S-<f8>" 'my/clojure-dev-reset
  :n "C-S-c" 'portal.api/clear
  ;; :n "C-S-c" 'my/clojure-reveal-clear-output

  )

(map!
  :leader
  (:prefix-map ("k" . "structural editing")
               "dx" #'kill-sexp
               "dX" #'backward-kill-sexp
               "c" #'lispy-clone
               "p" #'cljr-raise
               "D" #'cljr-destructure-keys
               ))

(map!
  :after clojure-mode
  :map (clojure-mode-map clojurescript-mode-map clojurec-mode-map)
  (:localleader
    (:prefix-map ("e" . "eval")
    "D" nil
    "D" 'my/eval-defun-and-reload-browser
    "c" 'my/eval-rcf
    ;; "e" 'cider-tap-last-sexp
    ;; "E" 'cider-eval-last-sexp
    )
    (:prefix-map ("n" . "namespace")
    "s" 'clojure-sort-ns
    )))

(map! :map doom-leader-map "o g" #'elpher)


;; Map expand & contract -region
(map! :nv "ghe" #'er/expand-region)
(map! :nv "ghi" #'er/contract-region)

(map!
  :n "<f2>" 'flycheck-next-error)
