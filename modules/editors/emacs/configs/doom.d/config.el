;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(setq custom-file                     (expand-file-name "custom-nixos.el"    user-emacs-directory))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-gruvbox)
                                        ;(setq doom-font (font-spec :family "IosevkaTerm Nerd Font Medium" :size 18))
(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 18))
(setq nerd-icons-font-names '("SymbolsNerdFontMono-Regular.ttf"))
                                        ;(setq doom-themes-treemacs-enable-variable-pitch nil)
(setq display-line-numbers-type 'relative)
;; (setq-default tab-width 2)
;; (setq-default indent-tabs-mode nil)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/docs/org/")

;; Nix managed authinfo
(add-to-list 'auth-sources "~/.authinfo")

;; Aggressive auto backup feature
;; I've been bit y undo corruption too many times to fully trust emacs with my
;; code. Since we don't have an intellij style Local History feature, we just
;; keep a ton of backups around.
(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook  'force-backup-of-buffer)

(setq create-lockfiles nil
      make-backup-files t     ; enable backup files
      vc-make-backup-files t  ; backup version controlled files
      version-control t       ; number each backup file
      delete-old-versions t   ; clean up after itself
      kept-old-versions 0     ; don't bother with old versions
      kept-new-versions 1000  ; keep 1000 latest versions
      backup-by-copying t    ; don't clobber symlinks
      backup-directory-alist (list
                              (cons tramp-file-name-regexp nil)
                              (cons "." (concat cache-dir "backup/"))
                              )
      tramp-backup-directory-alist nil
      )

;; Disable backup on certain sensitive file types
;; source: https://anirudhsasikumar.net/blog/2005.01.21.html
(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
        ;; disable backups
        (set (make-local-variable 'backup-inhibited) t)
        ;; disable auto-save
        (if auto-save-default
            (auto-save-mode -1)))
                                        ;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
                                        ;resort to default auto save setting
    (if auto-save-default
        (auto-save-mode 1))))

(setq auto-mode-alist
      (append '(("\\.gpg$" . sensitive-mode)
                ("\\.env$" . sensitive-mode)
                ("\\.job$" . hcl-mode)
                ("\\.hcl$" . hcl-mode)
                ("\\.bu$" . yaml-mode)
                )
              auto-mode-alist))

;; Projects all over
(setq projectile-project-search-path '(
                                       ("~/src" . 1)
                                       ("~/src/sno" . 1)
                                       ("~/src/many-stars" . 1)
                                       ("~/src/ovos" . 1)
                                       ("~/src/clojure-playground" . 1)
                                       ("~/work/vollers/src". 1)
                                       ))
;; show me projects in LIFO order
(setq projectile-sort-order 'recently-active)

;; define a function to set the ssh auth socket to the gpg agent
(defun set-ssh-auth-to-gpg-socket ()
  (interactive)
  (setenv
   "SSH_AUTH_SOCK"
   (string-chop-newline
    (shell-command-to-string "gpgconf --list-dirs | grep \"agent-ssh-socket\" | cut -d: -f2"))))

(after! magit
        (set-ssh-auth-to-gpg-socket))

(after! company
        (setq company-auto-complete t
              ;; complete on clojure namespaces
              company-auto-complete-chars "/"))

;; (after! treemacs
;;   ;; follow the current project file
;;   (setq treemacs-follow-mode t)
;;   (setq +treemacs-git-mode 'deferred)
;;   ;; single click expand/collapse nodes
;;   (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
;;   (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

(evil-define-command rm/lispyville-insert-at-end-of-list (count)
  "same as `lispyville-insert-at-end-of-list', but adds a newline."
  (interactive "<c>")
  ;; TODO if already at the end of the list, add a newline above
  (when (lispyville--out-forward (or count 1))
    (backward-char)
    (newline-and-indent)
    (evil-change-state lispyville-preferred-state)))

;; Lispyville needs Lispy, but I don't want to ever use the insane lispy-mode
;; bindings.
;; (use-package! lispy)
(map!
 :after lispyville
 :map lispyville-mode-map
 :n "M-L" #'lispyville-beginning-of-next-defun
 :v "(" #'lispy-parens)

(require 'evil-nerd-commenter)
(use-package! lispyville
              :hook ((lisp-mode . lispyville-mode)
                     (emacs-lisp-mode . lispyville-mode)
                     (ielm-mode . lispyville-mode)
                     (scheme-mode . lispyville-mode)
                     (racket-mode . lispyville-mode)
                     (hy-mode . lispyville-mode)
                     (lfe-mode . lispyville-mode)
                     (dune-mode . lispyville-mode)
                     (clojure-mode . lispyville-mode)
                     (fennel-mode . lispyville-mode))
              :init
              (setq
               lispyville-key-theme
               '(
                 (operators normal)
                 c-w c-u ;; Ctrl-w and Ctrl-u are sexp aware
                 prettify
                 text-objects
                 ;; (atom-movement normal visual)
                 ;; W, B and E will operate on atoms
                 ;; (atom-motions t)
                 atom-motions
                 additional-movement
                 ;; (additional-movement normal visual motion)
                 additional        ;; M-j/k to swap atoms forward/back
                 additional-insert ;;  M-{i,a,o,O} for sexp-aware enter insert
                 (additional-wrap normal insert)
                 (commentary normal visual)
                 slurp/barf-lispy ;; >/< to 'grow' and 'shrink' sexps
                 (escape insert emacs)))
              :config
              (lispyville-set-key-theme)

              (evil-define-key 'normal lispyville-mode-map
                (kbd "M-o") 'rm/lispyville-insert-at-end-of-list)

              (setq
               lispy-safe-actions-ignore-strings t
               lispy-safe-actions-ignore-comments t)

              (remove-hook 'clojure-mode-hook 'parinfer-mode)
              (remove-hook 'emacs-lisp-mode-hook 'parinfer-mode)

              ;; make w/e/b move by words inside strings and comments
              (defun fn--lispyville-e-handler ()
                (interactive)
                (if (or (in-string-p)
                        (evilnc-pure-comment-p (point))
                        )
                    (evil-forward-word-end)
                  (lispyville-forward-atom-end)))

              (defun fn--lispyville-w-handler ()
                (interactive)
                (if (or (in-string-p)
                        (evilnc-pure-comment-p (point))
                        )
                    (evil-forward-word-begin)
                  (lispyville-forward-atom-begin)))

              (defun fn--lispyville-b-handler ()
                (interactive)
                (if (or (in-string-p)
                        (evilnc-pure-comment-p (point)))
                    (evil-backward-word-begin)
                  (lispyville-backward-atom-begin)))

              (evil-define-key '(normal visual) lispyville-mode-map
                (kbd "e") 'fn--lispyville-e-handler
                (kbd "w") 'fn--lispyville-w-handler
                (kbd "b") 'fn--lispyville-b-handler)
              )


(use-package! elpher :defer t)
(after! elpher
        (map! (:localleader
               (:map elpher-mode-map)
               "b" #'elpher-back
               "r" #'elpher-reload
               "g" #'elpher-go
               "B" #'elpher-show-bookmarks
               "A" #'elpher-bookmark-current
               "U" #'elpher-copy-link-url
               "u" #'elpher-copy-current-url
               "d" #'elpher-download
               "." #'elpher-view-raw
               ))

        (add-hook 'elpher-mode-hook
                  (defun +elpher-clean-up-ui ()
                    (setq cursor-type nil)
                    (hl-line-mode -1)))
        (add-hook 'elpher-mode-hook #'doom-mark-buffer-as-real-h)
        (advice-add #'elpher-bookmark-jump :after
                    (defun +elpher-bookmark-jump-a (_)
                      (switch-to-buffer elpher-buffer-name))))



(after! flycheck
        ;; flycheck-next-error will navigate to errors before warnings
        (setq flycheck-navigation-minimum-level 'error))

;; ensure identically named files are shown in the buffer switcher with a directory disambiguation
(require 'uniquify)
;;(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


;; (use-package! python-black
;;   :demand t
;;   :after python
;;   :hook (python-mode . python-black-on-save-mode-enable-dwim))

(after!
 treemacs (treemacs-follow-mode 1))

;; (use-package! gptel
;;   :config
;;   (setq-default gptel-model "gpt-4")
;;   (setq gptel-model "gpt-4")
;;   (setq gptel-directives '((default . "You are a large language model living in Emacs and a helpful coding assistant. Respond concisely.")
;;                            (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
;;                            (writing . "You are a large language model and a writing assistant. Respond concisely.")
;;                            (chat . "You are a large language model and a conversation partner. Respond concisely."))))
;; source: https://www.armindarvish.com/en/post/use_emacs_as_a_chatgpt_client/
;; (defun ad/ai-from-anywhere ()
;;   (interactive)
;;   (let* ((screen-width (display-pixel-width))
;;          (screen-height (display-pixel-height))
;;          (frame-width (/ screen-width 3))
;;          (frame-height screen-height)
;;          (frame-left 0)
;;          (frame-top 0)
;;          (chat-frame (make-frame `((window-system . x)
;;                                    (top . ,frame-top)
;;                                    (left . ,frame-left)
;;                                    (width . (text-pixels . ,frame-width))
;;                                    (heigth . (text-pixels . ,frame-height))
;;                                    (name . "MyAI")
;;                                    (minibuffer . t)
;;                                    ))))
;;     (select-frame chat-frame)
;;     )

;;   (setq gptel-model "gpt-4")
;;   (add-hook 'gptel-post-response-hook (lambda () (goto-char (point-max))))
;;   (gptel "My:AI Chat" gptel-api-key nil)
;;   (switch-to-buffer "My:AI Chat")
;;   (delete-other-windows)
;;   )
;; (ad/ai-from-anywhere)

(with-eval-after-load 'tramp

  (set-ssh-auth-to-gpg-socket)
  (tramp-set-completion-function "ssh"
                                 '((tramp-parse-sconfig "~/.ssh/config.d/70-outskirts-labs")
                                   (tramp-parse-sconfig "~/.ssh/config.d/99-home-network")
                                   (tramp-parse-sconfig "~/.ssh/config.d/00-config")))

  (with-eval-after-load 'tramp-cache
    (setq tramp-persistency-file-name "~/.emacs.d/tramp"))
  (setq
   tramp-default-method "ssh"
   ;; clobber the default user list so it always prefers usernames from the ssh config
   tramp-default-user-alist
   '(("ssh" nil nil)
     ("ssh" ".*" nil)
     ("\\`\\(?:doas\\|ksu\\|su\\(?:do\\)?\\)\\'" nil "root")
     ("\\`smb\\'" nil nil)
     ("\\`sudoedit\\'" nil "root"))
   ;; use the settings in ~/.ssh/config instead of Tramp's
   tramp-use-ssh-controlmaster-options nil
   ;; don't generate backups for remote files opened as root (security hazzard)
   backup-enable-predicate
   (lambda (name)
     (and (normal-backup-enable-predicate name)
          (not (let ((method (file-remote-p name 'method)))
                 (when (stringp method)
                   (member method '("su" "sudo")))))))))


(defun tramp-abort ()
  (interactive)
  (recentf-cleanup)
  (tramp-cleanup-all-buffers)
  (tramp-cleanup-all-connections))

(after! tramp
        (set-ssh-auth-to-gpg-socket))

(defun my-vc-off-if-remote ()
  (if (file-remote-p (buffer-file-name))
      (setq-local vc-handled-backends '(Git))))

(add-hook 'find-file-hook 'my-vc-off-if-remote)

;; (setq vc-handled-backends '(Git))

(use-package! zoxide
              :defer t
              :init
              (defun +zoxide-add (&optional path &rest _)
                "Add PATH to zoxide database.  This function is called asynchronously."
                (interactive "Dpath: ")
                (unless path
                  (setq path default-directory))
                (zoxide-run t "add" path)

                (require 's)
                (let ((b (buffer-file-name)))
                  (unless (or (s-ends-with? ".git" b)
                              (s-contains? "/.git/" b))
                    (zoxide-run t "add" b))))
              (add-hook! 'find-file-hook #'+zoxide-add)
              (defvar consult-dir--source-zoxide
                `(:name "Zoxide dirs"
                        :narrow ?z
                        :category file
                        :face consult-file
                        :history file-name-history
                        :enabled ,(lambda () (executable-find "zoxide"))
                        :items ,#'zoxide-query)
                "Zoxide directory source for `consult-dir'.")
              (after! consult-dir
                      (pushnew! consult-dir-sources 'consult-dir--source-zoxide)))

(after! apheleia
        (setf (alist-get 'nixfmt apheleia-formatters)
              '("nixfmt" "--width=100")))


(load! "+bindings.el")
;; (load! "+dashboard.el")
(load! "+clojure.el")
;; (load! "+vterm.el")
(load! "+lsp.el")
(load! "+copilot.el")
;; (load! "+tabby.el")
