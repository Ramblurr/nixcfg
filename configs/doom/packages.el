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

;; (package! lsp-mode)
;; (package! lsp-ui :recipe (:host github :repo "emacs-lsp/lsp-ui"))
(package! transpose-frame)
;; (package! copilot :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")) :pin
;;           ;2025-09-16
;;           "6a2ad80489b8a0d021df95293eb7ac370aea140b"
;;           )
(package! aggressive-indent)
(package! symex)
(package! clojure-essential-ref)
(package! clojure-essential-ref-nov)
(package! clj-ns-name :recipe (:host github :repo "plexus/plexmacs" :files ("clj-ns-name/clj-ns-name.el")))
(package! eca :recipe (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")) :pin "27dfc334afeeb89cf3844831bc9a23e3ddcd309d")
(package! gptel :pin "81618f24e2190568ea745dca82ba50267eed321a") ;; v 0.9.9
(package! whisper :recipe (:host github :repo "natrys/whisper.el"))
(package! treemacs-all-the-icons)

(package! ajrepl
  :recipe (:type git
           :host github
           :repo "sogaiu/ajrepl"
           :files (:defaults ("ajrepl/"
                              "ajrepl/*"))))

(package! kdl-ts-mode :recipe (:host github :repo "merrickluo/kdl-ts-mode"))

(unpin! hover)
(unpin! treemacs)
(unpin! lsp-treemacs)
(unpin! lsp-ui)
