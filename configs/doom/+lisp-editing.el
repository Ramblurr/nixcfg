;;; $DOOMDIR/+lisp-editing.el -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;; Don't use smartparens global mode
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
;; Don't use smartparens in lisp modes (i use lispy and lispyville)
(dolist (mode '(lisp-mode
                emacs-lisp-mode
                ielm-mode
                scheme-mode
                racket-mode
                janet-mode
                hy-mode
                lfe-mode
                dune-mode
                clojure-mode
                fennel-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook")) #'turn-off-smartparens-mode))

(comment
 (use-package! aggressive-indent
   :hook
   (clojure-mode . aggressive-indent-mode)
   (emacs-lisp-mode . aggressive-indent-mode)
   (lisp-mode . aggressive-indent-mode)
   (fennel-mode . aggressive-indent-mode)))

(comment
 :config
 (setq clojure-indent-style 'align-arguments)
 (setq clojure-align-forms-automatically t)
 (setq cider-dynamic-indentation t
       cider-font-lock-dynamically nil
       cider-font-lock-reader-conditionals nil))

;; evil-nerd-commenter must be loaded before lispyville
(use-package! evil-nerd-commenter)

;; ----------------------------------------------------------------------------------
;; Lispy + Lispyville
;; ----------------------------------------------------------------------------------
;; Ok, so what's up with all these different lisp-specific editing packages?
;; lispy, lispyville, and symex.. where is your standard parinfer, [smart|clever]parens?
;; Well, I have *desires* about how I want editing lisp to work.
;; I don't want to have to worry about parens, but I generally want to be able to use all my
;; normal vim commands, dd, di, x, etc.
(comment (after! lispy
           ;; notably absent from this list is 'special', which is the bit of lispy that provides the
           ;; context-specific editing mode. I don't want that (which is maybe sad, since that is 90% of lispy), but
           ;; rather, I want all the other benefits.
           (lispy-set-key-theme '(lispy c-digits))
           (setq lispy-safe-actions-ignore-strings t
                 lispy-safe-actions-ignore-comments t)
           (add-hook 'lispy-mode-hook (lambda () (if lispy-mode (hl-line-mode -1) (when (bound-and-true-p global-hl-line-mode) (hl-line-mode +1)))))))

(comment (add-hook! lispy-mode 'my/dim-parens)
         (add-hook! lispy-mode 'my/fade-characters))

(comment (after! lispyville
           (setq
            lispy-safe-actions-ignore-strings t
            lispy-safe-actions-ignore-comments t)
           (setq
            lispyville-key-theme
            '(
              c-w c-u                       ; Ctrl-w and Ctrl-u are sexp aware
              prettify                      ; =/TAB will prettify the sexp
              text-objects                  ; sexp text objects
              atom-motions                  ; W, B and E will operate on atoms
              additional                    ; M-j/k to drag atoms forward/back
              additional-movement ; lots here, but i mainly use M-h lispyville-beginning-of-defun, M-l lispyville-end-of-defun
              additional-insert ; M-{i,a,o,O} for sexp-aware enter insert begin, end, above, below
              (additional-wrap normal insert) ; M-([{ for wrapping sexp
              slurp/barf-lispy                ; >/< to 'grow' and 'shrink' sexps
              (commentary normal visual)
              (escape insert emacs)))
           (lispyville-set-key-theme)

           (evil-define-key 'normal lispyville-mode-map
             (kbd "M-o") 'my/lispyville-insert-at-end-of-list)


           (remove-hook 'clojure-mode-hook 'parinfer-mode)
           (remove-hook 'emacs-lisp-mode-hook 'parinfer-mode)

           ;; In lisp buffers, I want w,e,b to go forward-backward on the atom level
           ;; 'atom-motions' in lispyville provides this feature.
           ;; But, then it jumps over entore comments and strings, which is not what I want
           ;; so these special handlers to work on atoms when we are not inside comments
           ;; or strings, but when we are inside one of those, then behave like normal vi
           (evil-define-key '(normal visual) lispyville-mode-map
             (kbd "e") 'fn--lispyville-e-handler
             (kbd "w") 'fn--lispyville-w-handler
             (kbd "b") 'fn--lispyville-b-handler)))

;; ----------------------------------------------------------------------------------
;; symex - Intuituve s-expression editing
;; ----------------------------------------------------------------------------------
;; provides an alternative evil state for editing s-expressions

(comment
 (defvar-local entered-insert-from-symex nil
   "Did buffer's most recent entry into insert state come from symex state?"))

(use-package! symex-core)
(use-package! symex
  :config
  (symex-mode 1)
  (defun my/enter-symex-mode () (hl-line-mode -1))
  (defun my/exit-symex-mode () (hl-line-mode 0))
  (add-hook! symex-editing-mode 'my/dim-parens)
  (add-hook! symex-editing-mode 'my/fade-characters)
  (add-hook! 'symex-editing-mode-pre-entry-hook 'my/enter-symex-mode)
  (add-hook! 'symex-editing-mode-post-exit-hook 'my/exit-symex-mode)
  (defun my/remember-if-coming-from-symex ()
    (setq entered-insert-from-symex (eq evil-previous-state 'symex)))

  (defun  my/possibly-restore-symex-state (&rest _)
    ;; when we're coming from insert state, and got thither from symex state
    (when (and (eq evil-previous-state 'insert) entered-insert-from-symex)
      (symex-mode-interface)))

  (add-hook! 'evil-insert-state-entry-hook 'my/remember-if-coming-from-symex)
  ;; when returning from insert state, go back to symex state if it's whence we came
  (advice-add 'evil-normal-state :after #'my/possibly-restore-symex-state))


(use-package! symex-ide
  :after (symex)
  :config (symex-ide-mode 1))

(use-package! symex-evil
  :after (symex evil)
  :config
  (add-to-list 'doom-leader-key-states 'symex)
  (symex-evil-mode 1)
  (lithium-define-keys symex-editing-mode
    ((">" symex-capture-forward)
     ("<" symex-capture-backward))))

(comment (use-package! symex
           :commands (symex-mode symex-mode-interface)
           :custom
           (symex-modal-backend 'evil)
           :config
           (setq symex--user-evil-keyspec
                 '(("j" . symex-go-forward)
                   ("k" . symex-go-backward)
                   ("l" . symex-go-up)
                   ("h" . symex-go-down)
                   ("C-j" . symex-climb-branch)
                   ("C-k" . symex-descend-branch)
                   ("C-a" . symex-goto-first)
                   ("C-e" . symex-goto-last)
                   ("M-j" . symex-goto-highest)
                   ("M-k" . symex-goto-lowest)
                   (">" . symex-capture-forward)
                   ("<" . symex-capture-backward)
                   ("[" . symex-soar-backward)
                   ("]" . symex-soar-forward)
                   ("{" . symex-create-square)
                   ("}" . symex-wrap-square)
                   ("M-[" . symex-leap-backward)
                   ("M-]" . symex-leap-forward)))

           ;; in symex-mode, lispy should be off...
           (defun +symex-mode-interface ()
             (lispy-mode -1)
             (lispyville-mode -1))
           (add-hook! 'evil-symex-state-entry-hook '+symex-mode-interface)

           ;; ..but back in evil normal mode we want lispyville activated
           (defun +symex-state-exit ()
             (lispy-mode 1))
           (add-hook! 'evil-symex-state-exit-hook '+symex-state-exit)

           (remove-hook! 'evil-symex-state-exit-hook #'symex-disable-editing-minor-mode)

           ;; ESC will toggle between symex and normal mode
           (defadvice! +evil-force-normal-state (orig)
             :around #'evil-force-normal-state
             (if (and (not (evil-symex-state-p)) lispy-mode)
                 (symex-mode-interface)))

           (custom-theme-set-faces! '(doom-gruvbox)
             `(symex--current-node-face :background ,(doom-color 'bg-alt2)))
           (symex-initialize)

           (defadvice! +symex-evaluate (orig-fn count)
             "evaluate at right paren"
             :around #'symex-evaluate
             (interactive "p")
             (if (eq (char-after) 41)
                 (save-excursion
                   (evil-jump-item)
                   (call-interactively orig-fn count))
               (call-interactively orig-fn count)))
           ))
