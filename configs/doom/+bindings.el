
(map!
 ;; intellij muscle memory dies hard
 :n "C-e" #'persp-switch-to-buffer
 :n "C-n" #'consult-lsp-symbols
 :n "<f2>" #'flycheck-next-error
 :n "<f5>" #'consult-lsp-diagnostics
 :n "<f12>" #'consult-lsp-file-symbols
 )

(map!
 :leader
 "gx" #'my/copy-latest-commit-hash

 (:prefix-map ("l" . "LLMs/AI")
  :desc "Select model" :n "m" #'my/gptel-select-default-backend
  :desc "Open gptel Buffer" :n "q" #'gptel
  :desc "Submit prompt to AT" :n "a" #'gptel-send
  :desc "Send entire buffer to AI" :n "b" #'gptel-ext-send-whole-buffer
  :desc "Load buffer into session" :n "l" #'gptel-ext-ask-document
  :desc "Rewrite region" :n "R" #'gptel-ext-rewrite-and-replace
  :desc "Refactor" :n "r" #'gptel-ext-refactor
  :desc "ECA toggle" :n "e" #'eca-chat-toggle-window
  :desc "ECA Add current file" :n "f" #'eca-chat-add-file-context
  :desc "ECA Drop current file" :n "d" #'eca-chat-drop-file-context
  :desc "ECA Add current selection" :n "s" #'eca-chat-add-context-at-point
  :desc "ECA Talk" :n "t" #'eca-chat-talk
  :desc "Whisper" :n "w" #'my/whisper-english-hydra/body
  ))

(map!
 :leader
 (:prefix ("k" . "Structural Editing")
          "dx" #'kill-sexp
          "dX" #'backward-kill-sexp
          "c" #'lispy-clone
          "p" #'lispy-raise
          "D" #'cljr-destructure-keys
          "ks" #'my/destructure-clojure-param
          ))

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
 :after lispyville
 :map lispyville-mode-map
 :n "M-L" #'lispyville-beginning-of-next-defun
 :v "(" #'lispy-parens)


(map!
 :after eca
 :map eca-chat-mode-map
 (:localleader
  :desc "Clear" :n "c" #'eca-chat-clear
  :desc "Reset" :n "r" #'eca-chat-reset
  :desc "Talk" :n "t" #'eca-chat-talk
  :desc "Select model" :n "m" #'eca-chat-select-model
  :desc "Change behavior" :n "b" #'eca-chat-select-behavior
  :desc "Open/close chat window" :n "o" #'eca-chat-toggle-window
  :desc "Accept next pending tool call" :n "a" #'eca-chat-tool-call-accept-next
  :desc "Accept all pending tool calls" :n "A" #'eca-chat-tool-call-accept-all
  :desc "MCP Details" :n "M" #'eca-mcp-details
  :desc "Show stderr (logs)" :n "E" #'eca-show-stderr
  :desc "Restart" :n "R" #'eca-restart
  :desc "Stop" :n "S" #'eca-stop))
