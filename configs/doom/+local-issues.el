;;; +local-issues.el --- Browse project-local Org issues -*- lexical-binding: t; -*-

;; Project-local issue browsing and agenda support.

(defvar ramblurr/project-scratch-org-directory-name ".scratch-org"
  "Project-relative directory containing local Org issue files.")

(defvar ramblurr/project-scratch-work-item-limit 10
  "Number of recent work items shown before the index is collapsed.")

(defvar-local ramblurr/project-scratch-show-all-work-items nil
  "Non-nil means show every work item in this agenda buffer.")

(defconst ramblurr/project-scratch-todo-sequence
  '(sequence
    "NEEDS-TRIAGE"
    "NEEDS-INFO"
    "READY-FOR-AGENT"
    "READY-FOR-HUMAN"
    "IN-PROGRESS"
    "CLAIMED"
    "|"
    "RESOLVED"
    "WONTFIX")
  "TODO sequence used by project-local issues.")

(defun ramblurr/project-scratch-org-directory ()
  "Return the current project's local Org issue directory."
  (let ((project (project-current nil)))
    (unless project
      (user-error "Current buffer is not inside a project"))
    (let ((directory
           (expand-file-name
            ramblurr/project-scratch-org-directory-name
            (project-root project))))
      (unless (file-directory-p directory)
        (user-error "No %s directory in this project"
                    ramblurr/project-scratch-org-directory-name))
      (file-name-as-directory directory))))

(defun ramblurr/project-scratch-org-files ()
  "Return every Org file beneath the current project's issue directory."
  (directory-files-recursively
   (ramblurr/project-scratch-org-directory)
   "\\.org\\'"))

(defun ramblurr/project-scratch-work-item-directories ()
  "Return numbered work-item directories in the current project."
  (let ((root (ramblurr/project-scratch-org-directory))
        work-items)
    (dolist (entry
             (directory-files
              root
              t
              "\\`[0-9]\\{3\\}-.+\\'"))
      (when (file-directory-p entry)
        (push entry work-items)))
    (sort work-items #'string>)))

(defun ramblurr/org-agenda-open-work-item-button (button)
  "Open BUTTON's work-item directory in Dired."
  (dired (button-get button 'ramblurr/work-item-directory)))

(defun ramblurr/org-agenda-toggle-work-items ()
  "Toggle between recent and all work items in the current agenda."
  (interactive)
  (setq-local ramblurr/project-scratch-show-all-work-items
              (not ramblurr/project-scratch-show-all-work-items))
  (ramblurr/org-agenda-insert-work-items)
  (goto-char (point-min)))

(defun ramblurr/org-agenda-toggle-work-items-button (_button)
  "Toggle the work-item index from a text button."
  (ramblurr/org-agenda-toggle-work-items))

(defun ramblurr/org-agenda-open-at-point ()
  "Open the work item or Org entry at point in another window."
  (interactive)
  (let ((directory
         (get-text-property
          (line-beginning-position)
          'ramblurr/work-item-directory)))
    (if directory
        (dired-other-window directory)
      (org-agenda-goto))))

(defun ramblurr/org-agenda-insert-work-items ()
  "Insert a directory-derived work-item index into the project agenda."
  (when (equal org-agenda-name "Project issues")
    (let* ((inhibit-read-only t)
           (existing
            (text-property-any
             (point-min)
             (point-max)
             'ramblurr/work-item-index
             t))
           (work-items
            (ramblurr/project-scratch-work-item-directories))
           (total (length work-items))
           (collapsed
            (and (not ramblurr/project-scratch-show-all-work-items)
                 (> total ramblurr/project-scratch-work-item-limit)))
           (visible-work-items
            (if collapsed
                (seq-take
                 work-items
                 ramblurr/project-scratch-work-item-limit)
              work-items))
           (hidden-count (- total (length visible-work-items))))
      (when existing
        (delete-region
         existing
         (or (next-single-property-change
              existing
              'ramblurr/work-item-index
              nil
              (point-max))
             (point-max))))
      (goto-char (point-min))
      (let ((section-start (point)))
        (insert
         (propertize
          (cond
           (collapsed
            (format "Work items — latest %d of %d\n"
                    (length visible-work-items)
                    total))
           ((and ramblurr/project-scratch-show-all-work-items
                 (> total ramblurr/project-scratch-work-item-limit))
            (format "Work items — all %d\n" total))
           (t
            (format "Work items — %d\n" total)))
          'face
          'org-agenda-structure))
        (dolist (directory visible-work-items)
          (let* ((name
                  (file-name-nondirectory
                   (directory-file-name directory)))
                 (number (substring name 0 3))
                 (title
                  (capitalize
                   (replace-regexp-in-string
                    "-"
                    " "
                    (substring name 4))))
                 (line-start (point)))
            (insert (format " %-4s %s" number title))
            (make-text-button
             line-start
             (point)
             'action #'ramblurr/org-agenda-open-work-item-button
             'follow-link t
             'face 'default
             'mouse-face 'highlight
             'help-echo name
             'ramblurr/work-item-directory directory)
            (add-text-properties
             line-start
             (point)
             `(ramblurr/work-item-directory ,directory))
            (insert "\n")))
        (when (> total ramblurr/project-scratch-work-item-limit)
          (let ((line-start (point)))
            (insert
             (if collapsed
                 (format "      %d older work items hidden — show all"
                         hidden-count)
               (format "      Show latest %d work items"
                       ramblurr/project-scratch-work-item-limit)))
            (make-text-button
             line-start
             (point)
             'action #'ramblurr/org-agenda-toggle-work-items-button
             'follow-link t
             'face 'shadow
             'mouse-face 'highlight
             'help-echo "Toggle older work items")
            (insert "\n")))
        (insert "\n")
        (add-text-properties
         section-start
         (point)
         '(ramblurr/work-item-index t))))))

(defun ramblurr/org-agenda-item-id ()
  "Return the current agenda item's canonical identifier.

Tickets use their composite work-item and ticket number, such as `006-02'.
Other work-item documents use the work-item number and basename, such as
`001-spec'."
  (let* ((base (or (buffer-base-buffer) (current-buffer)))
         (file (buffer-file-name base)))
    (cond
     ((and file
           (string-match
            "/\\([0-9]\\{3\\}\\)-[^/]+/issues/\\([0-9]\\{2\\}\\)-"
            file))
      (concat (match-string 1 file)
              "-"
              (match-string 2 file)))
     ((and file
           (string-match
            "/\\([0-9]\\{3\\}\\)-[^/]+/\\([^/]+\\)\\.org\\'"
            file))
      (concat (match-string 1 file)
              "-"
              (match-string 2 file)))
     (t ""))))

(defun ramblurr/org-agenda-item-prefix ()
  "Return a fixed-width identifier for the current agenda item."
  (format "%-16.16s" (ramblurr/org-agenda-item-id)))

(defun ramblurr/org-agenda-source-path ()
  "Return the current agenda row's work-item-relative path."
  (let* ((work-item-directory
          (get-text-property
           (line-beginning-position)
           'ramblurr/work-item-directory))
         (marker
          (or (org-get-at-bol 'org-hd-marker)
              (org-get-at-bol 'org-marker)))
         (file
          (and (markerp marker)
               (buffer-live-p (marker-buffer marker))
               (buffer-file-name (marker-buffer marker))))
         (directory-pattern
          (concat "/"
                  (regexp-quote ramblurr/project-scratch-org-directory-name)
                  "/\\(.*\\)\\'")))
    (or (and work-item-directory
             (file-name-nondirectory
              (directory-file-name work-item-directory)))
        (when (and file (string-match directory-pattern file))
          (replace-regexp-in-string
           "/issues/"
           "/"
           (match-string 1 file)
           t
           t)))))

(defun ramblurr/org-agenda-source-mode-line ()
  "Configure source previews and the agenda buffer's mode line."
  ;; Org's normal display fallback may reuse the agenda window.  Follow and
  ;; preview mode must preserve that window and use a second one instead.
  (setq-local
   display-buffer-overriding-action
   '((display-buffer-reuse-window display-buffer-pop-up-window)
     (inhibit-same-window . t)))
  (setq-local
   mode-line-buffer-identification
   '(" "
     (:eval
      (or (ramblurr/org-agenda-source-path)
          "Project issues")))))

(defun ramblurr/project-scratch-agenda ()
  "Open the local-issues agenda for the current project."
  (interactive)
  (org-agenda nil "S"))

(defun ramblurr/project-scratch-find ()
  "Find an Org file beneath the current project's issue directory."
  (interactive)
  (let* ((root (ramblurr/project-scratch-org-directory))
         (files
          (mapcar
           (lambda (file)
             (file-relative-name file root))
           (ramblurr/project-scratch-org-files))))
    (unless files
      (user-error "No Org issue files found"))
    (find-file
     (expand-file-name
      (completing-read "Project issue: " files nil t)
      root))))


(defconst ramblurr/project-scratch-agenda-command
  '("S" "Project issues"
    ((todo "NEEDS-TRIAGE|NEEDS-INFO"
           ((org-agenda-overriding-header "Triage")
            (org-agenda-prefix-format
             " %(ramblurr/org-agenda-item-prefix)")
            (org-agenda-todo-keyword-format "%-16s")))
     (todo "READY-FOR-AGENT|READY-FOR-HUMAN"
           ((org-agenda-overriding-header "Ready")
            (org-agenda-prefix-format
             " %(ramblurr/org-agenda-item-prefix)")
            (org-agenda-todo-keyword-format "%-16s")))
     (todo "IN-PROGRESS|CLAIMED"
           ((org-agenda-overriding-header "Active")
            (org-agenda-prefix-format
             " %(ramblurr/org-agenda-item-prefix)")
            (org-agenda-todo-keyword-format "%-16s")))
     (todo "RESOLVED|WONTFIX"
           ((org-agenda-overriding-header "Closed")
            (org-agenda-prefix-format
             " %(ramblurr/org-agenda-item-prefix)")
            (org-agenda-todo-keyword-format "%-16s"))))
    ((org-agenda-files (ramblurr/project-scratch-org-files))))
  "Custom agenda command for project-local issues.")

(after! org
  (add-to-list 'org-todo-keywords ramblurr/project-scratch-todo-sequence)

  ;; Refresh TODO recognition when this file is re-evaluated.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (org-set-regexps-and-options)))))

(after! org-agenda
  ;; Re-evaluation replaces the previous command instead of duplicating it.
  (setq org-agenda-custom-commands
        (cons ramblurr/project-scratch-agenda-command
              (assoc-delete-all "S" org-agenda-custom-commands)))

  (add-hook 'org-agenda-mode-hook #'ramblurr/org-agenda-source-mode-line)
  (add-hook 'org-agenda-finalize-hook #'ramblurr/org-agenda-insert-work-items)

  (map! :map org-agenda-mode-map
        :localleader
        :desc "Preview item"             "p" #'org-agenda-show-and-scroll-up
        :desc "Open item"                "o" #'ramblurr/org-agenda-open-at-point
        :desc "Toggle all work items"    "w" #'ramblurr/org-agenda-toggle-work-items
        :desc "Toggle follow mode"       "f" #'org-agenda-follow-mode
        :desc "Close preview windows"    "c" #'delete-other-windows
        :desc "Refresh agenda"           "r" #'org-agenda-redo
        :desc "Quit agenda"              "q" #'org-agenda-quit)

  ;; Update an already-open project agenda when this file is re-evaluated.
  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (when (buffer-live-p agenda-buffer)
      (with-current-buffer agenda-buffer
        (when (derived-mode-p 'org-agenda-mode)
          (ramblurr/org-agenda-source-mode-line)
          (ramblurr/org-agenda-insert-work-items)
          (force-mode-line-update))))))

(provide '+local-issues)

;;; +local-issues.el ends here
