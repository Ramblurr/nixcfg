;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defmacro comment (&rest body)
  "Ignore BODY, yields nil."
  nil)

;; Fonts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq doom-font (font-spec :family "Iosevka SS15" :size 16))
(setq nerd-icons-font-names '("Iosevka Nerd Font Mono" "Symbols Nerd Font"))

;; get the family string from the doom-font variable containing the font-spec

(setq confirm-kill-emacs nil)

;;(setq doom-theme 'doom-gruvbox)
(setq doom-theme 'modus-vivendi)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(setq doom-gruvbox-brighter-comments t)
(setq doom-gruvbox-dark-variant 'hard)
(setq display-line-numbers-type 'relative)

(defface my-face-faded nil
  "Faded face is for information that are less important.")

(use-package! modus-themes
  :init
  (defun my/modus-themes-custom-faces (&rest _)
    (modus-themes-with-colors
      (custom-set-faces
       ;; Make foreground the same as background for a uniform bar on
       ;; Doom Emacs.
       ;;
       ;; Doom should not be implementing such hacks because themes
       ;; cannot support them:
       ;; <https://protesilaos.com/codelog/2022-08-04-doom-git-gutter-modus-themes/>.
       `(git-gutter-fr:added ((,c :foreground ,bg-added-fringe)))
       `(git-gutter-fr:deleted ((,c :foreground ,bg-removed-fringe)))
       `(git-gutter-fr:modified ((,c :foreground ,bg-changed-fringe))))))
  (add-hook! 'modus-themes-after-load-theme-hook 'my/modus-themes-custom-faces)

  (setq!
   modus-themes-completions '((matches . (extrabold background intense))
                              (selection . (semibold accented intense))
                              (popup . (accented)))
   modus-themes-common-palette-overrides
   '((bg-paren-match bg-magenta-intense)
     (underline-paren-match fg-main) )
   )

  (comment
   (custom-theme-set-faces! '(modus-vivendi)
     `(symex-highlight-face     :inherit ,nil :background ,(modus-themes-get-color-value 'bg-active) :weight ,'normal)
     `(clojure-discard-face     :foreground ,(modus-themes-get-color-value 'bg-active))
     `(my-face-faded            :foreground ,(modus-themes-get-color-value 'fg-dim)))))


(custom-theme-set-faces! '(doom-gruvbox)
  `(symex-highlight-face     :background ,(doom-color 'base3) :weight ,'bold)
  `(show-paren-match         :foreground ,(doom-color 'orange) :background ,(doom-color 'base3) :weight ,'extra-bold)
  `(cursor                   :background ,(doom-color 'base8))
  `(my-face-faded            :background ,(doom-color 'bg) :foreground ,(doom-color 'grey))
  `(shadow                   :background ,(doom-color 'bg) :foreground ,(doom-color 'base4))
  `(line-number              :background ,(doom-color 'bg) :foreground ,(doom-color 'base5))
  ;; `(font-lock-comment-face   :foreground ,(doom-color 'green))
  ;; `(font-lock-constant-face  :foreground ,(doom-color 'violet))
  ;; `(font-lock-builtin-face   :foreground ,(doom-color 'orange))
  ;; `(font-lock-keyword-face   :inherit ,'default :foreground ,'unspecified :weight ,'unspecified)
  )


(defun my/clojure-faces ()
  (comment
   (setq-local +my/face-remap-cookies
               (list
                (face-remap-add-relative 'font-lock-comment-face :foreground (doom-color 'yellow))
                (face-remap-add-relative 'font-lock-doc-face :foreground (doom-color 'yellow))
                ;; (face-remap-add-relative 'font-lock-constant-face :inherit 'default)
                (face-remap-add-relative 'font-lock-function-name-face :foreground (doom-color 'teal))
                (face-remap-add-relative 'font-lock-string-face :foreground (doom-color 'blue))
                (face-remap-add-relative 'font-lock-number-face :foreground (doom-color 'blue))
                ;; (face-remap-add-relative 'lsp-face-highlight-textual :foreground (doom-color 'yellow))
                (face-remap-add-relative 'clojure-keyword-face :foreground (doom-color 'violet))
                (face-remap-add-relative 'font-lock-keyword-face :inherit 'default :foreground 'unspecified)
                ;; (face-remap-add-relative 'font-lock-keyword-face :foreground (doom-color 'teal))

                )       )))

(defun my/dim-parens ()
  "Make parenthesis less prominent by matching comment face."
  (font-lock-add-keywords nil `((,(rx (any "()")) . 'my-face-faded))))

(defun my/fade-characters ()
  "Make some characters less prominent."
  (font-lock-add-keywords nil `((,(rx (any "[]{}_&#%~@.,")) . 'my-face-faded))))


(setq org-directory "~/docs/org/")


;; Aggressive auto backup feature
;; I've been bit y undo corruption too many times to fully trust emacs with my
;; code. Since we don't have an intellij style Local History feature, we just
;; keep a ton of backups around.
(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook  'force-backup-of-buffer)


;; Projects all over
(setq projectile-project-search-path '(("~/src" . 2)
                                       ("~/work" . 3)))

;; Show me projects in LIFO order
(setq projectile-sort-order 'recently-active)

;; ensure identically named files are shown in the buffer switcher with a directory disambiguation
(use-package! uniquify
  :config
  ;;(setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  )

(after! flycheck
  ;; flycheck-next-error will navigate to errors before warnings
  (setq flycheck-navigation-minimum-level 'error))


                                        ;(after! apheleia
                                        ;  (setf (alist-get 'nixfmt apheleia-formatters)
                                        ;        '("nixfmt" "--width=100")))

(after! tree-sitter
  (setq treesit-language-source-alist
        '(
          (fluent . ("https://github.com/tree-sitter/tree-sitter-fluent")))))


(after! hl-line
  (setq! hl-line-sticky-flag nil))

(use-package ligature
  :config
  ;; Enable all Iosevka ligatures in programming modes
  ;; https://github.com/mickeynp/ligature.el/wiki
  (defvar iosevka-ligation-set '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                 "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                 "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                 ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  (defvar berkeley-mono-ligation-set '(; Group A
                                       ".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
                                       ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
                                        ; Group B
                                       "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
                                       "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
                                       "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
                                       "|=" "//=" "/="
                                        ; Group C
                                       "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
                                       "<:<" ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
                                       "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
                                        ; Group D
                                       "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
                                        ; Group E
                                       "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
                                       "{!--" "//" "///" "!!"
                                        ; Group F
                                       "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"
                                        ; Group G
                                       "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
                                       "--" "---"))
  (defun setup-ligatures-for-font ()
    (let ((font-family (symbol-name (font-get doom-font :family))))
      (cond
       ((and font-family (string-match-p "Iosevka" font-family))
        (message "Setting up ligatures for Iosevka")
        (ligature-set-ligatures 'prog-mode iosevka-ligation-set))

       ((and font-family (string-match-p "Berkeley Mono" font-family))
        (message "Setting up ligatures for Berkeley Mono")
        (ligature-set-ligatures 'prog-mode berkeley-mono-ligation-set))

       (t (message "No specific ligature set for %s" font-family)))))

  (setup-ligatures-for-font)

  ;; Enables ligature checks globally in all buffers. You can also do it per
  ;; mode with `ligature-mode'.
  (global-ligature-mode t))

(load! "+helpers.el")
(load! "+my-commands.el")
(load! "+org.el")
(load! "+ai.el")
(load! "+lisp-editing.el")
(load! "+langs.el")
(load! "+clojure.el")
(load! "+emacs-wm.el")
(load! "+bindings.el")
