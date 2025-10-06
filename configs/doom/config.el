;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defmacro comment (&rest body)
  "Ignore BODY, yields nil."
  nil)

;; Fonts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 18))
;; (setq doom-font (font-spec :family "Iosevka SS08" :size 16))
(setq doom-font (font-spec :family "Iosevka SS09" :size 16))
;; (setq doom-font (font-spec :family "Iosevka SS15" :size 16))
;; (setq doom-font (font-spec :family "Iosevka Etoile" :size 16))
;; (setq doom-font (font-spec :family "Berkeley Mono Trial" :size 15))
;; (setq doom-variable-pitch-font (font-spec :family "Berkeley Mono Trial" :size 15))
(setq nerd-icons-font-names '("Iosevka Nerd Font Mono" "Symbols Nerd Font"))

;; get the family string from the doom-font variable containing the font-spec

(setq confirm-kill-emacs nil)

(setq doom-theme 'doom-gruvbox)

(custom-theme-set-faces! '(doom-gruvbox)
  `(cursor                   :background ,(doom-color 'orange))
  `(shadow                   :background ,(doom-color 'bg) :foreground ,(doom-color 'base4))
  `(line-number              :background ,(doom-color 'bg) :foreground ,(doom-color 'base5)))

(setq display-line-numbers-type 'relative)

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
(load! "+ai.el")
(load! "+lisp-editing.el")
(load! "+langs.el")
(load! "+clojure.el")
(load! "+emacs-wm.el")
(load! "+bindings.el")
