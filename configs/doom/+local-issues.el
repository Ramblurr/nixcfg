;;; +local-issues.el --- Browse project-local Org issues -*- lexical-binding: t; -*-

;; Project-local issue browsing and agenda support.

(defvar my/project-scratch-org-directory-name ".scratch-org"
  "Project-relative directory containing local Org issue files.")

(defvar my/project-scratch-work-item-limit 10
  "Number of recent work items shown before the index is collapsed.")

(defconst my/project-scratch-closed-page-size 10
  "Number of closed entries revealed per agenda page.")

(defvar my/project-scratch-load-more-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (dolist (key '("RET" "<return>" "<kp-enter>" "SPC"))
      (define-key map (kbd key) #'push-button))
    map)
  "Keymap for activating the closed-ticket paging row.")

(defvar my/project-scratch-agenda-entry-map
  (let ((map (make-sparse-keymap)))
    (dolist (key '("RET" "<return>" "<kp-enter>"))
      (define-key map (kbd key) #'my/org-agenda-open-at-point))
    map)
  "Keymap for opening a rendered project agenda entry.")

(defvar-local my/project-scratch-agenda-entries nil
  "Entries from the most recent project agenda scan.")

(defvar-local my/project-scratch-visible-closed-count
    my/project-scratch-closed-page-size
  "Number of closed entries currently visible in this agenda buffer.")
(defvar-local my/project-scratch-show-all-work-items nil
  "Non-nil means show every work item in this agenda buffer.")

(defconst my/project-scratch-todo-sequence
  '(sequence
    "NEEDS-TRIAGE"
    "NEEDS-INFO"
    "READY-FOR-AGENT"
    "READY-FOR-HUMAN"
    "IN-PROGRESS"
    "CLAIMED"
    "DEFERRED"
    "|"
    "RESOLVED"
    "WONTFIX")
  "TODO sequence used by project-local issues.")

(defun my/project-scratch-org-directory ()
  "Return the current project's local Org issue directory."
  (let ((project (project-current nil)))
    (unless project
      (user-error "Current buffer is not inside a project"))
    (let ((directory
           (expand-file-name
            my/project-scratch-org-directory-name
            (project-root project))))
      (unless (file-directory-p directory)
        (user-error "No %s directory in this project"
                    my/project-scratch-org-directory-name))
      (file-name-as-directory directory))))

(defun my/project-scratch-org-files ()
  "Return every Org file beneath the current project's issue directory."
  (directory-files-recursively
   (my/project-scratch-org-directory)
   "\\.org\\'"))

(defun my/project-scratch-work-item-directories ()
  "Return numbered work-item directories in the current project."
  (let ((root (my/project-scratch-org-directory))
        work-items)
    (dolist (entry
             (directory-files
              root
              t
              "\\`[0-9]\\{3\\}-.+\\'"))
      (when (file-directory-p entry)
        (push entry work-items)))
    (sort work-items #'string>)))

(defun my/project-scratch-agenda-files ()
  "Return saved Org files that belong in the project agenda."
  (let (files)
    (dolist (work-item (my/project-scratch-work-item-directories))
      (dolist (basename '("spec.org" "map.org"))
        (let ((file (expand-file-name basename work-item)))
          (when (file-regular-p file)
            (push file files))))
      (let ((issues (expand-file-name "issues" work-item)))
        (when (file-directory-p issues)
          (dolist (file (directory-files issues t "\\.org\\'"))
            (when (file-regular-p file)
              (push file files))))))
    (sort files #'string<)))

(defun my/project-scratch-prepare-agenda-files ()
  "Return agenda files after replacing matching buffers from disk."
  (let ((files (my/project-scratch-agenda-files)))
    (dolist (file files)
      (let ((buffer (find-buffer-visiting file)))
        (when buffer
          (with-current-buffer buffer
            (revert-buffer t t t)))))
    files))

(defconst my/project-scratch-agenda-sections
  '(("Triage" "NEEDS-TRIAGE" "NEEDS-INFO")
    ("Ready" "READY-FOR-AGENT" "READY-FOR-HUMAN")
    ("Active" "IN-PROGRESS" "CLAIMED")
    ("Deferred" "DEFERRED")
    ("Closed" "RESOLVED" "WONTFIX"))
  "Ordered TODO states displayed in the project scratch agenda.")

(defun my/project-scratch--scan-agenda-file (file scratch-buffer)
  "Return agenda entries read from FILE once using SCRATCH-BUFFER."
  (let* ((visited (find-buffer-visiting file))
         (buffer (or visited scratch-buffer))
         entries)
    (with-current-buffer buffer
      (let ((buffer-file-name (if visited buffer-file-name file)))
        (unwind-protect
            (progn
              (unless visited
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert-file-contents file)))
              (org-with-wide-buffer
               (org-map-entries
                (lambda ()
                  (let ((todo (org-get-todo-state)))
                    (when todo
                      (push
                       (list :todo todo
                             :todo-face (org-get-todo-face todo)
                             :title (org-get-heading t t t t)
                             :id (my/org-agenda-item-id)
                             :scheduled (org-entry-get nil "SCHEDULED")
                             :deadline (org-entry-get nil "DEADLINE")
                             :file file
                             :position (line-beginning-position))
                       entries))))
                nil
                'file)))
          (unless visited
            (set-buffer-modified-p nil)))))
    (nreverse entries)))

(defun my/project-scratch--planning-time (timestamp)
  "Return the time represented by Org TIMESTAMP, or nil."
  (and timestamp (ignore-errors (org-time-string-to-time timestamp))))

(defun my/project-scratch--deferred-entry-less-p (left right)
  "Return non-nil when deferred entry LEFT is scheduled before RIGHT."
  (let ((left-time
         (my/project-scratch--planning-time (plist-get left :scheduled)))
        (right-time
         (my/project-scratch--planning-time (plist-get right :scheduled))))
    (cond
     ((and left-time right-time)
      (if (equal left-time right-time)
          (string< (plist-get left :id) (plist-get right :id))
        (time-less-p left-time right-time)))
     (left-time t)
     (t nil))))

(defun my/project-scratch--closed-entry-less-p (left right)
  "Return non-nil when closed entry LEFT has a later identifier than RIGHT."
  (string> (plist-get left :id) (plist-get right :id)))

(defun my/project-scratch--schedule-status (entry)
  "Return a visible due status for deferred ENTRY, or nil."
  (let* ((scheduled (plist-get entry :scheduled))
         (time (my/project-scratch--planning-time scheduled))
         (now (current-time)))
    (when (and (equal (plist-get entry :todo) "DEFERRED")
               time
               (not (time-less-p now time)))
      (if (equal (format-time-string "%F" time)
                 (format-time-string "%F" now))
          "[DUE]"
        "[OVERDUE]"))))

(defun my/project-scratch--planning-label (entry)
  "Return visible scheduling and deadline text for ENTRY."
  (mapconcat
   #'identity
   (delq nil
         (list
          (my/project-scratch--schedule-status entry)
          (and (plist-get entry :scheduled)
               (concat "SCHEDULED: " (plist-get entry :scheduled)))
          (and (plist-get entry :deadline)
               (concat "DEADLINE: " (plist-get entry :deadline)))))
   " "))

(defun my/project-scratch--insert-agenda-entry (entry)
  "Insert one rendered project agenda ENTRY."
  (let ((start (point))
        (planning (my/project-scratch--planning-label entry)))
    (insert
     (format " %-16s %-16s %s%s\n"
             (plist-get entry :id)
             (propertize
              (plist-get entry :todo)
              'face
              (plist-get entry :todo-face))
             (if (equal planning "") "" (concat planning " "))
             (plist-get entry :title)))
    (add-text-properties
     start
     (point)
     `(my/agenda-source-file ,(plist-get entry :file)
                             my/agenda-source-position ,(plist-get entry :position)
                             keymap ,my/project-scratch-agenda-entry-map
                             mouse-face highlight))))

(defun my/project-scratch--insert-load-more-closed (remaining)
  "Insert a button revealing another page from REMAINING closed entries."
  (let ((start (point))
        (next-count
         (min my/project-scratch-closed-page-size remaining)))
    (insert
     (format "      %d older closed tickets remain — show next %d"
             remaining
             next-count))
    (make-text-button
     start
     (point)
     'action #'my/project-scratch-load-more-closed-button
     'keymap my/project-scratch-load-more-button-map
     'follow-link t
     'face 'shadow
     'mouse-face 'highlight
     'help-echo "Reveal older closed tickets")
    (insert "\n")))

(defun my/project-scratch--render-agenda ()
  "Render the current project agenda without scanning its sources."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (section my/project-scratch-agenda-sections)
      (let* ((name (car section))
             (section-entries
              (seq-filter
               (lambda (entry)
                 (member (plist-get entry :todo) (cdr section)))
               my/project-scratch-agenda-entries)))
        (insert
         (propertize (concat name "\n") 'face 'org-agenda-structure))
        (cond
         ((equal name "Deferred")
          (setq section-entries
                (sort section-entries
                      #'my/project-scratch--deferred-entry-less-p)))
         ((equal name "Closed")
          (setq section-entries
                (sort section-entries
                      #'my/project-scratch--closed-entry-less-p))))
        (let* ((closed-p (equal name "Closed"))
               (visible-entries
                (if closed-p
                    (seq-take
                     section-entries
                     my/project-scratch-visible-closed-count)
                  section-entries))
               (remaining (- (length section-entries)
                             (length visible-entries))))
          (dolist (entry visible-entries)
            (my/project-scratch--insert-agenda-entry entry))
          (when (> remaining 0)
            (my/project-scratch--insert-load-more-closed remaining)))
        (insert "\n")))
    (goto-char (point-min)))
  (my/org-agenda-source-mode-line)
  (my/org-agenda-insert-work-items)
  (setq buffer-read-only t)
  (current-buffer))

(defun my/project-scratch-load-more-closed-button (button)
  "Reveal the next page of closed entries without rescanning source files."
  (let ((closed-total
         (seq-count
          (lambda (entry)
            (member (plist-get entry :todo) '("RESOLVED" "WONTFIX")))
          my/project-scratch-agenda-entries)))
    (setq-local
     my/project-scratch-visible-closed-count
     (min closed-total
          (+ my/project-scratch-visible-closed-count
             my/project-scratch-closed-page-size)))
    (my/project-scratch--render-agenda)
    (let ((destination
           (or
            (text-property-any
             (point-min)
             (point-max)
             'keymap
             my/project-scratch-load-more-button-map)
            (save-excursion
              (goto-char (point-max))
              (skip-chars-backward "\n")
              (line-beginning-position)))))
      (set-marker button destination)
      (goto-char destination))))

(defun my/project-scratch-agenda-view (&optional _match)
  "Build the project agenda from each saved source file once."
  (let* ((files (my/project-scratch-prepare-agenda-files))
         (entries
          (with-temp-buffer
            (delay-mode-hooks (org-mode))
            (mapcan
             (lambda (file)
               (my/project-scratch--scan-agenda-file file (current-buffer)))
             files))))
    (org-agenda-prepare "Project issues")
    (setq-local org-agenda-files files)
    (setq-local my/project-scratch-agenda-entries entries)
    (setq-local my/project-scratch-visible-closed-count
                my/project-scratch-closed-page-size)
    (my/project-scratch--render-agenda)))

(defun my/org-agenda-open-work-item-button (button)
  "Open BUTTON's work-item directory in Dired."
  (dired (button-get button 'my/work-item-directory)))

(defun my/org-agenda-toggle-work-items ()
  "Toggle between recent and all work items in the current agenda."
  (interactive)
  (setq-local my/project-scratch-show-all-work-items
              (not my/project-scratch-show-all-work-items))
  (my/org-agenda-insert-work-items)
  (goto-char (point-min)))

(defun my/org-agenda-toggle-work-items-button (_button)
  "Toggle the work-item index from a text button."
  (my/org-agenda-toggle-work-items))

(defun my/org-agenda--visit-source (&optional other-window)
  "Visit the project agenda source at point in OTHER-WINDOW when non-nil."
  (let ((file
         (get-text-property
          (line-beginning-position)
          'my/agenda-source-file))
        (position
         (get-text-property
          (line-beginning-position)
          'my/agenda-source-position)))
    (unless (and file position)
      (user-error "No project issue at point"))
    (if other-window
        (find-file-other-window file)
      (find-file file))
    (goto-char position)
    (org-show-context)))

(defun my/org-agenda-preview-at-point ()
  "Preview the project agenda source at point in another window."
  (interactive)
  (save-selected-window
    (my/org-agenda--visit-source t)))

(defun my/org-agenda-open-at-point ()
  "Open the work item or Org entry at point in another window."
  (interactive)
  (let ((directory
         (get-text-property
          (line-beginning-position)
          'my/work-item-directory)))
    (if directory
        (dired-other-window directory)
      (my/org-agenda--visit-source t))))

(defun my/org-agenda-insert-work-items ()
  "Insert a directory-derived work-item index into the project agenda."
  (when (equal org-agenda-name "Project issues")
    (let* ((inhibit-read-only t)
           (existing
            (text-property-any
             (point-min)
             (point-max)
             'my/work-item-index
             t))
           (work-items
            (my/project-scratch-work-item-directories))
           (total (length work-items))
           (collapsed
            (and (not my/project-scratch-show-all-work-items)
                 (> total my/project-scratch-work-item-limit)))
           (visible-work-items
            (if collapsed
                (seq-take
                 work-items
                 my/project-scratch-work-item-limit)
              work-items))
           (hidden-count (- total (length visible-work-items))))
      (when existing
        (delete-region
         existing
         (or (next-single-property-change
              existing
              'my/work-item-index
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
           ((and my/project-scratch-show-all-work-items
                 (> total my/project-scratch-work-item-limit))
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
             'action #'my/org-agenda-open-work-item-button
             'follow-link t
             'face 'default
             'mouse-face 'highlight
             'help-echo name
             'my/work-item-directory directory)
            (add-text-properties
             line-start
             (point)
             `(my/work-item-directory ,directory))
            (insert "\n")))
        (when (> total my/project-scratch-work-item-limit)
          (let ((line-start (point)))
            (insert
             (if collapsed
                 (format "      %d older work items hidden — show all"
                         hidden-count)
               (format "      Show latest %d work items"
                       my/project-scratch-work-item-limit)))
            (make-text-button
             line-start
             (point)
             'action #'my/org-agenda-toggle-work-items-button
             'follow-link t
             'face 'shadow
             'mouse-face 'highlight
             'help-echo "Toggle older work items")
            (insert "\n")))
        (insert "\n")
        (add-text-properties
         section-start
         (point)
         '(my/work-item-index t))))))

(defun my/org-agenda-item-id ()
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

(defun my/org-agenda-item-prefix ()
  "Return a fixed-width identifier for the current agenda item."
  (format "%-16.16s" (my/org-agenda-item-id)))

(defun my/org-agenda-source-path ()
  "Return the current agenda row's work-item-relative path."
  (let* ((work-item-directory
          (get-text-property
           (line-beginning-position)
           'my/work-item-directory))
         (marker
          (or (org-get-at-bol 'org-hd-marker)
              (org-get-at-bol 'org-marker)))
         (file
          (or
           (get-text-property
            (line-beginning-position)
            'my/agenda-source-file)
           (and (markerp marker)
                (buffer-live-p (marker-buffer marker))
                (buffer-file-name (marker-buffer marker)))))
         (directory-pattern
          (concat "/"
                  (regexp-quote my/project-scratch-org-directory-name)
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

(defun my/org-agenda-source-mode-line ()
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
      (or (my/org-agenda-source-path)
          "Project issues")))))

(defun my/project-scratch-agenda ()
  "Open the local-issues agenda for the current project."
  (interactive)
  (org-agenda nil "S"))

(defun my/project-scratch-agenda-refresh ()
  "Refresh the current project scratch agenda from saved files."
  (interactive)
  (unless (equal org-agenda-name "Project issues")
    (user-error "Current buffer is not the project issues agenda"))
  (my/project-scratch-agenda-view))

(defun my/org-agenda-redo ()
  "Refresh the current agenda through its supported rendering path."
  (interactive)
  (if (equal org-agenda-name "Project issues")
      (my/project-scratch-agenda-refresh)
    (org-agenda-redo)))

(defun my/project-scratch--finder-completion-table (files work-items)
  "Return a completion table for relative FILES and WORK-ITEMS."
  (lambda (input predicate action)
    (let* ((scope-prefix
            (and (string-match "\\`[0-9]\\{3\\} " input)
                 (match-string 0 input)))
           (scope-id (and scope-prefix (substring scope-prefix 0 3)))
           (work-item
            (and scope-id
                 (seq-find
                  (lambda (directory)
                    (string-prefix-p
                     (concat scope-id "-")
                     (file-name-nondirectory
                      (directory-file-name directory))))
                  work-items))))
      (if (not work-item)
          (complete-with-action action files input predicate)
        (let* ((directory-prefix
                (concat
                 (file-name-nondirectory
                  (directory-file-name work-item))
                 "/"))
               (scoped-files
                (seq-filter
                 (lambda (file)
                   (string-prefix-p directory-prefix file))
                 files)))
          (funcall
           (completion-table-subvert scoped-files scope-prefix "")
           input
           predicate
           action))))))

(defun my/project-scratch-find ()
  "Find an Org file beneath the current project's issue directory."
  (interactive)
  (let* ((root (my/project-scratch-org-directory))
         (files
          (mapcar
           (lambda (file)
             (file-relative-name file root))
           (my/project-scratch-org-files))))
    (unless files
      (user-error "No Org issue files found"))
    (find-file
     (expand-file-name
      (completing-read
       "Project issue: "
       (my/project-scratch--finder-completion-table
        files
        (my/project-scratch-work-item-directories))
       nil
       t)
      root))))


(defconst my/project-scratch-agenda-command
  '("S" "Project issues" my/project-scratch-agenda-view "")
  "Custom agenda command for project-local issues.")

(after! org
  (add-to-list 'org-todo-keywords my/project-scratch-todo-sequence)

  ;; Refresh TODO recognition when this file is re-evaluated.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (org-set-regexps-and-options)))))

(after! org-agenda
  ;; Re-evaluation replaces the previous command instead of duplicating it.
  (setq org-agenda-custom-commands
        (cons my/project-scratch-agenda-command
              (assoc-delete-all "S" org-agenda-custom-commands)))

  (add-hook 'org-agenda-mode-hook #'my/org-agenda-source-mode-line)
  (add-hook 'org-agenda-finalize-hook #'my/org-agenda-insert-work-items)

  (define-key org-agenda-mode-map (kbd "r") #'my/org-agenda-redo)

  (map! :map org-agenda-mode-map
        :localleader
        :desc "Preview item"             "p" #'my/org-agenda-preview-at-point
        :desc "Open item"                "o" #'my/org-agenda-open-at-point
        :desc "Toggle all work items"    "w" #'my/org-agenda-toggle-work-items
        :desc "Toggle follow mode"       "f" #'org-agenda-follow-mode
        :desc "Close preview windows"    "c" #'delete-other-windows
        :desc "Refresh agenda"           "r" #'my/project-scratch-agenda-refresh
        :desc "Quit agenda"              "q" #'org-agenda-quit)

  ;; Update an already-open project agenda when this file is re-evaluated.
  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (when (buffer-live-p agenda-buffer)
      (with-current-buffer agenda-buffer
        (when (derived-mode-p 'org-agenda-mode)
          (my/org-agenda-source-mode-line)
          (my/org-agenda-insert-work-items)
          (force-mode-line-update))))))

(provide '+local-issues)

;;; +local-issues.el ends here
