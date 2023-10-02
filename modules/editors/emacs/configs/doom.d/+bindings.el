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
  :leader
  (:prefix-map ("a" . "AI/ChatGPT")
    :desc "Open gptel Buffer" :n "q" #'gptel
    :desc "Submit prompt to AT" :n "a" #'gptel-send
    :desc "Send entire buffer to AI" :n "b" #'gptel-ext-send-whole-buffer
    :desc "Load buffer into session" :n "l" #'gptel-ext-ask-document
    :desc "Rewrite region" :n "R" #'gptel-ext-rewrite-and-replace
    :desc "Refactor" :n "r" #'gptel-ext-refactor
    )
  )



(map! :map doom-leader-map "o g" #'elpher)


;; Map expand & contract -region
(map! :nv "ghe" #'er/expand-region)
(map! :nv "ghi" #'er/contract-region)

(map!
  :n "<f2>" 'flycheck-next-error)
