;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

(defconst is-android (string-equal system-type "android"))
(defconst not-android (not is-android))
(when not-android
  (package! transpose-frame)

  (unpin! hover)
  (unpin! treemacs)
  (unpin! lsp-treemacs)
  (unpin! lsp-ui)
  (unpin! lsp-mode)

  (package! aggressive-indent)
  ;; (package! clojure-essential-ref)
  ;; (package! clojure-essential-ref-nov)
  (package! clj-ns-name :recipe (:host github :repo "plexus/plexmacs" :files ("clj-ns-name/clj-ns-name.el")))
  (package! eca :recipe (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")) :pin "de940cc5203b79f1bd9c8581fa91b5fdfafdc86f")
  (package! gptel :pin "33ed0f6f69857157973bca72fb4d454e29965bdd")
  (package! whisper :recipe (:host github :repo "natrys/whisper.el") :pin "fd9bf5787a99dd31a4bdf54d2bd9821aacf84e93")
  (package! treemacs-all-the-icons)

  (package! ajrepl :recipe (:type git :host github :repo "sogaiu/ajrepl" :files (:defaults ("ajrepl/" "ajrepl/*"))))
  ;; (package! kdl-ts-mode :recipe (:host github :repo "merrickluo/kdl-ts-mode"))

  (package! fluent-ts-mode
    :recipe (:type nil
              :local-repo "/home/ramblurr/src/github.com/outskirtslabs/fluent-tooling"
              :files ("editors/emacs/*.el")))

  (package! symex-core
    :recipe (:host github
              :repo "drym-org/symex.el"
              :files ("symex-core/symex*.el" ))
    :pin "4025d17ba260c893d58848258647005b58964b42")

  (package! symex
    :recipe (:host github
              :repo "drym-org/symex.el"
              :files ("symex/symex*.el" "symex/doc/*.texi" "symex/doc/figures"))
    :pin "4025d17ba260c893d58848258647005b58964b42")

  (package! symex-evil
    :recipe (:host github
              :repo "drym-org/symex.el"
              :files ("symex-evil/symex*.el"))
    :pin "4025d17ba260c893d58848258647005b58964b42")

  (package! symex-ide
    :recipe (:host github
              :repo "drym-org/symex.el"
              :files ("symex-ide/symex*.el"))
    :pin "4025d17ba260c893d58848258647005b58964b42")

  (package! modus-themes
    :recipe (:host github :repo "protesilaos/modus-themes"))

  ;; (package! mcp
  ;;   :recipe (:host github :repo "lizqwerscott/mcp.el")
  ;;   :pin "2632a53ba35332aaa8fbfffa11db17c52304ec51")

  (package! org-roam-ui)
  (package! consult-flyspell)
  (package! magit-difftastic
    :recipe (:host github :repo "rschmukler/magit-difftastic")))

(package! org-modern-indent :recipe
  (:host github
    :repo "jdtsmith/org-modern-indent"))
(package! org-ql :pin "4b8330a683c43bb4a2c64ccce8cd5a90c8b174ca")
