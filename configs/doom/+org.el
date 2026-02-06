;;; ../../nixcfg/configs/doom/+org.el -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(defun my/set-org-title (title)
  "Update the Org-mode buffer's title or insert it if missing."
  (interactive "sEnter new title: ")
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not a org-mode buffer"))
  (save-excursion
    (goto-char (point-min))
    (forward-line 7)
    (let ((bound (line-end-position)))
      (goto-char (point-min))
      (if (re-search-forward "^#\\+TITLE:[ \t]*.*" bound t)
          (replace-match
           (format "#+TITLE: %s" title))
        (goto-char (point-min))
        (insert (format "#+TITLE: %s\n" title))))))

(defun my/set-org-top-header (title)
  "Update the Org-mode buffer's title or insert it if missing."
  (interactive "sEnter new title: ")
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not a org-mode buffer"))
  (save-excursion
    (goto-char (point-min))
    (forward-line 7)
    (let ((bound (line-end-position)))
      (goto-char (point-min))
      (if (re-search-forward "^\\*\\s.*" bound t)
          (replace-match
           (format "* %s" title))
        (goto-char (point-min))
        (insert (format "* %s\n" title))))))

(defconst my/org-link-colors
  '(("http"  . blue)
    ("https" . blue)
    ("file"  . orange)
    ("pdf"   . orange)
    ("id"    . cyan))
  "Alist of Org link prefixes and their Doom theme colours.")

(defun my/org-apply-link-colors ()
  "Register custom faces for Org link schemes based on `my/org-link-colors'."
  (dolist (entry my/org-link-colors)
    (let ((scheme (car entry))
          (color  (cdr entry)))
      (org-link-set-parameters
       scheme
       :face       `(:foreground ,(doom-color color))
       :mouse-face `(:foreground ,(doom-color 'bg)
                     :background ,(doom-color color))))))

(setq org-directory "~/docs/org/"
      org-use-property-inheritance t
      org-startup-with-inline-images t
      org-edit-src-content-indentation 0 )

(after! org
  (custom-set-faces!
    `(org-document-title :foreground ,(doom-color 'fg) :height 1.3 :weight bold)
    `(org-level-1 :inherit 'outline-1 :weight medium   :height 1.1)
    `(org-level-2 :inherit 'outline-2 :weight medium)
    `(org-level-3 :inherit 'outline-3 :weight medium)
    `(org-level-4 :inherit 'outline-4 :weight medium)
    `(org-level-5 :inherit 'outline-5 :weight medium)))

(after! org
  (my/org-apply-link-colors)
  (custom-set-faces!
    `((org-link)
      :foreground ,(doom-color 'violet)
      :weight normal
      :underline nil)))

;; modified from https://sophiebos.io/posts/beautifying-emacs-org-mode/
(after! org-modern
  (setq org-auto-align-tags t
        org-tags-column 0
        org-fold-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t

        org-modern-table nil ;; use a different package in `other' for pretty tables
        org-modern-todo nil
        org-modern-priority nil
        org-modern-progress 6

        ;; agenda
        org-agenda-tags-column 0
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────"

        ;; text replacements
        org-modern-fold-stars
        `(("◉" . "○" )
          ("◈" . "◇" )
          ("◉" . "○" )
          ("◈" . "◇" )
          ("◉" . "○" ))
        org-modern-checkbox
        ;; requires nerd font
        '((88 . "󰄵")
          (45 . "󰡖")
          (32 . "󰄱"))
        org-modern-list
        '((43 . "•")
          (45 . "–")
          (42 . "↪")))

  (comment (custom-set-faces!
             `(org-modern-tag
               :background ,(doom-color 'fg-alt)
               :foreground ,(doom-color 'bg-alt)
               :family ,my/sans-serif-font
               :height 0.7))))

(after! org-appear
  (setq org-hide-emphasis-markers t
        org-pretty-entities nil
        ;; org-appear-autoentities t
        org-appear-autosubmarkers t
        org-appear-inside-latex t
        org-appear-autolinks 'just-brackets))

(use-package! org-roam
  :defer t
  :custom
  (org-roam-directory (file-truename "~/docs/org"))
  (org-roam-dailies-directory "daily")
  (org-roam-db-location (file-truename "~/docs/org/org-roam.db"))
  (org-attach-id-dir "assets/")
  (org-roam-dailies-capture-templates
   `(("d" "default" entry
      "* %?"
      :if-new (file+head ,my/daily-note-filename
                         ,my/daily-note-header)
      :unnarrowed t)
     ("t" "task" entry
      "* TODO %?\n  %U\n  %a\n  %i"
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("Tasks"))
      :empty-lines 1
      :unnarrowed t)
     ("l" "log entry" entry
      "* %<%I:%M %p> - %?"
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("Log"))
      :unnarrowed t)
     ("j" "journal" entry
      "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("Log"))
      :unnarrowed t)
     ("m" "meeting" entry
      "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("Log"))
      :unnarrowed t)))
  :config
  (setq  my/daily-note-filename "%<%Y-%m-%d>.org"
         my/daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
  (org-roam-db-autosync-enable)
  ;; custom `org-roam' functions
  (defun my/org-roam--after-point ()
    "If in Evil normal state and not at EOL, move one char forward."
    (when (and (bound-and-true-p evil-mode)
               (evil-normal-state-p)
               (not (eolp)))
      (forward-char)))

  (defun my/current-buffer-has-module-tag ()
    "Return non-nil when this buffer’s #+filetags line contains :module:."
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^#\\+filetags:.*:module:" nil t)))

  (defun my/org-roam-insert-lowercase ()
    "Insert an org-roam link after point.
   Lower-case the link text unless the current buffer is tagged :module:."
    (interactive)
    (let* ((module-context-p (my/current-buffer-has-module-tag))
           (org-roam-node-formatter
            (lambda (node)
              (let ((title (org-roam-node-title node)))
                (if module-context-p title (downcase title))))))
      (my/org-roam--after-point)
      (call-interactively #'org-roam-node-insert)))

  (defun my/org-roam-insert-custom-title ()
    "Pick a node, then prompt for a verbatim link description."
    (interactive)
    (my/org-roam--after-point)
    (let* ((node (org-roam-node-read))
           (desc (read-string "Description: "))
           (link (org-link-make-string
                  (concat "id:" (org-roam-node-id node))
                  desc)))
      (insert link)
      (org-roam-link-replace-at-point link)
      (run-hooks 'org-roam-insert-node-hook)
      (forward-char)))

  ;; copied from https://hieuphay.com/doom-emacs-config/#customizing-main-interface
  (defun my/org-roam-node-find-by-mtime ()
    "Find a node by last modified date."
    (interactive)
    (find-file
     (org-roam-node-file
      (org-roam-node-read nil nil #'org-roam-node-read-sort-by-file-mtime))))

  ;; keybindings
  (map! :map org-roam-mode-map
        :leader
        (:prefix ("r" . "roam")
         :desc "Add alias"          "a" #'org-roam-alias-add
         :desc "Remove alias"       "A" #'org-roam-alias-remove
         :desc "Toggle roam buffer" "b" #'org-roam-buffer-toggle
         :desc "Find node"          "f" #'my/org-roam-node-find-by-mtime
         :desc "Insert node"        "i" #'my/org-roam-insert-lowercase
         :desc "Insert title"       "I" #'my/org-roam-insert-custom-title
         :desc "Add tag"            "t" #'org-roam-tag-add
         :desc "Remove tag"         "T" #'org-roam-tag-remove
         :desc "Visit node"         "v" #'org-roam-node-visit)))

(use-package! websocket :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
        org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section))

  (map! :map org-roam-mode-map
        :leader
        (:prefix ("r" . "roam")
         :desc "Open ORUI" "u" #'org-roam-ui-open)))


(use-package! org-ql
  :when (modulep! :lang org)
  :defer t
  :commands org-ql-search
  :init (set-popup-rule! "^\\*Org QL View:" :side 'bottom :size .5 :select t :quit 'current)
  (map! (:leader (:prefix "s" :desc "Org QL Search" :ng "g" #'org-ql-search))))
