;;; local-issues-core.el --- Read project-local Org issue metadata -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'subr-x)

(defconst local-issues--todo-states
  '("NEEDS-TRIAGE"
    "NEEDS-INFO"
    "READY-FOR-AGENT"
    "READY-FOR-HUMAN"
    "IN-PROGRESS"
    "CLAIMED"
    "RESOLVED"
    "WONTFIX"))

(defconst local-issues--closed-states '("RESOLVED" "WONTFIX"))

(defconst local-issues--help
  "Usage: local-issues [--root PATH] COMMAND\n\nCommands:\n  list       List canonical tickets\n\nOptions:\n  --root PATH  Use PATH instead of discovering the tracker root\n  --help       Show this help\n")

(defconst local-issues--list-help
  "Usage: local-issues [--root PATH] list [OPTIONS]\n\nOptions:\n  --all             Include closed tickets\n  --work-item NNN   Restrict output to one work item\n  --format FORMAT   Use table or json output (default: table)\n  --help            Show this help\n")

(defun local-issues--repository-root (&optional override)
  "Return the tracker root at OVERRIDE or above `default-directory'."
  (if override
      (let ((root (expand-file-name override)))
        (unless (file-directory-p (expand-file-name ".scratch-org" root))
          (error "no .scratch-org directory under %s" root))
        (file-name-as-directory root))
    (let ((directory (file-name-as-directory (expand-file-name default-directory)))
          found)
      (while (and directory (not found))
        (if (file-directory-p (expand-file-name ".scratch-org" directory))
            (setq found directory)
          (let ((parent (file-name-directory (directory-file-name directory))))
            (setq directory (unless (equal parent directory) parent)))))
      (or found (error "no .scratch-org directory found from %s" default-directory)))))

(defun local-issues--work-items (root)
  "Return canonical work-item IDs beneath ROOT."
  (let ((scratch (expand-file-name ".scratch-org" root))
        work-items)
    (dolist (path (directory-files scratch t "\\`[0-9]\\{3\\}-.+\\'"))
      (when (file-directory-p path)
        (push (substring (file-name-nondirectory path) 0 3) work-items)))
    (sort (delete-dups work-items) #'string<)))

(defun local-issues--ticket-files (root)
  "Return canonical ticket paths beneath ROOT."
  (let ((scratch (expand-file-name ".scratch-org" root))
        files)
    (dolist (work-item (directory-files scratch t "\\`[0-9]\\{3\\}-.+\\'"))
      (let ((issues (expand-file-name "issues" work-item)))
        (when (and (file-directory-p work-item) (file-directory-p issues))
          (dolist (ticket (directory-files issues t "\\`[0-9]\\{2\\}-.+\\.org\\'"))
            (when (file-regular-p ticket)
              (push ticket files))))))
    files))

(defun local-issues--expected-id (path)
  "Return the canonical ticket ID implied by PATH."
  (unless (string-match
           "/\\([0-9]\\{3\\}\\)-[^/]+/issues/\\([0-9]\\{2\\}\\)-[^/]+\\.org\\'"
           path)
    (error "noncanonical ticket path %s" path))
  (concat (match-string 1 path) "-" (match-string 2 path)))

(defun local-issues--canonical-id-p (value)
  "Return non-nil when VALUE is a canonical ticket ID."
  (and value (string-match-p "\\`[0-9]\\{3\\}-[0-9]\\{2\\}\\'" value)))

(defun local-issues--add-diagnostic (record code message &optional invalid)
  "Add CODE and MESSAGE to RECORD, marking it INVALID when requested."
  (unless (cl-find-if
           (lambda (diagnostic)
             (and (equal code (plist-get diagnostic :code))
                  (equal message (plist-get diagnostic :message))))
           (plist-get record :diagnostics))
    (setf (plist-get record :diagnostics)
          (cons (list :code code :message message)
                (plist-get record :diagnostics))))
  (when invalid
    (setf (plist-get record :invalid) t)))

(defun local-issues--parse-ticket (path)
  "Read PATH from disk and return its first top-level heading metadata."
  (let* ((expected-id (local-issues--expected-id path))
         (record (list :id expected-id
                       :declared-id nil
                       :sort-id expected-id
                       :path path
                       :work-item (substring expected-id 0 3)
                       :title ""
                       :todo ""
                       :assignee ""
                       :blockers nil
                       :unresolved-blockers nil
                       :dependency nil
                       :diagnostics nil
                       :invalid nil)))
    (with-temp-buffer
      (insert-file-contents path)
      (delay-mode-hooks (org-mode))
      (goto-char (point-min))
      (if (not (re-search-forward "^\\* " nil t))
          (local-issues--add-diagnostic
           record "missing-heading" "missing top-level heading" t)
        (beginning-of-line)
        (let* ((heading-parts
                (split-string (org-get-heading t t t t) "[[:space:]]+" t))
               (state (or (car heading-parts) ""))
               (title (string-join (cdr heading-parts) " "))
               (id (org-entry-get nil "TICKET_ID"))
               (blocker-value (org-entry-get nil "BLOCKED_BY"))
               (assignee-value (org-entry-get nil "ASSIGNEE"))
               (blockers (split-string (or blocker-value "") "[[:space:]]+" t)))
          (setf (plist-get record :id) (or id expected-id)
                (plist-get record :declared-id) id
                (plist-get record :sort-id)
                (if (local-issues--canonical-id-p id) id expected-id)
                (plist-get record :title) title
                (plist-get record :todo) state
                (plist-get record :assignee) (string-trim (or assignee-value ""))
                (plist-get record :blockers) blockers)
          (unless (member state local-issues--todo-states)
            (local-issues--add-diagnostic
             record "unknown-todo" (format "unknown TODO state %s" state) t))
          (unless (local-issues--canonical-id-p id)
            (local-issues--add-diagnostic
             record "malformed-id" (format "malformed TICKET_ID %s" (or id "<missing>")) t))
          (when (and (local-issues--canonical-id-p id)
                     (not (equal id expected-id)))
            (local-issues--add-diagnostic
             record "path-id-mismatch"
             (format "ticket ID %s does not match path ID %s" id expected-id)
             t))
          (dolist (blocker blockers)
            (unless (local-issues--canonical-id-p blocker)
              (local-issues--add-diagnostic
               record "malformed-blocker"
               (format "malformed blocker %s" blocker)
               t)))
          (when (null assignee-value)
            (local-issues--add-diagnostic
             record "missing-assignee" "missing ASSIGNEE property")))))
    record))

(defun local-issues--record-table (records)
  "Return a hash table from declared canonical IDs to RECORDS."
  (let ((by-id (make-hash-table :test #'equal)))
    (dolist (record records)
      (let ((id (plist-get record :declared-id)))
        (when (local-issues--canonical-id-p id)
          (puthash id (cons record (gethash id by-id)) by-id))))
    by-id))

(defun local-issues--single-record (by-id id)
  "Return the unique record for ID in BY-ID, or nil."
  (let ((matches (gethash id by-id)))
    (when (= (length matches) 1)
      (car matches))))

(defun local-issues--mark-cycles (records by-id)
  "Mark dependency-cycle diagnostics in RECORDS using BY-ID."
  (let ((visits (make-hash-table :test #'equal)))
    (cl-labels
        ((visit
          (id stack)
          (pcase (gethash id visits)
            ('done nil)
            ('visiting
             (let* ((position (cl-position id stack :test #'equal))
                    (cycle (sort (cl-subseq stack 0 (1+ position)) #'string<))
                    (message (format "dependency cycle: %s" (string-join cycle ","))))
               (dolist (cycle-id cycle)
                 (local-issues--add-diagnostic
                  (local-issues--single-record by-id cycle-id)
                  "dependency-cycle" message t))))
            (_
             (puthash id 'visiting visits)
             (let ((record (local-issues--single-record by-id id)))
               (dolist (blocker (plist-get record :blockers))
                 (when (local-issues--single-record by-id blocker)
                   (visit blocker (cons id stack)))))
             (puthash id 'done visits)))))
      (dolist (record records)
        (let ((id (plist-get record :declared-id)))
          (when (and (local-issues--canonical-id-p id)
                     (local-issues--single-record by-id id))
            (visit id nil)))))))

(defun local-issues--validate-and-resolve (records)
  "Validate dependency relationships in RECORDS and add derived fields."
  (let ((by-id (local-issues--record-table records)))
    (maphash
     (lambda (id matches)
       (when (> (length matches) 1)
         (dolist (record matches)
           (local-issues--add-diagnostic
            record "duplicate-id" (format "duplicate ticket ID %s" id) t))))
     by-id)
    (dolist (record records)
      (dolist (blocker (plist-get record :blockers))
        (when (local-issues--canonical-id-p blocker)
          (let ((matches (gethash blocker by-id)))
            (cond
             ((null matches)
              (local-issues--add-diagnostic
               record "unknown-blocker"
               (format "unknown blocker %s" blocker) t))
             ((> (length matches) 1)
              (local-issues--add-diagnostic
               record "ambiguous-blocker"
               (format "ambiguous blocker %s" blocker) t)))))))
    (local-issues--mark-cycles records by-id)
    (let ((changed t))
      (while changed
        (setq changed nil)
        (dolist (record records)
          (unless (plist-get record :invalid)
            (dolist (blocker (plist-get record :blockers))
              (let ((blocked-on (local-issues--single-record by-id blocker)))
                (when (and blocked-on (plist-get blocked-on :invalid))
                  (local-issues--add-diagnostic
                   record "invalid-blocker"
                   (format "readiness depends on invalid ticket %s" blocker) t)
                  (setq changed t))))))))
    (dolist (record records)
      (let ((unresolved
             (cl-remove-if
              (lambda (id)
                (let ((blocker (local-issues--single-record by-id id)))
                  (and blocker
                       (not (plist-get blocker :invalid))
                       (equal "RESOLVED" (plist-get blocker :todo)))))
              (plist-get record :blockers))))
        (setf (plist-get record :unresolved-blockers) unresolved
              (plist-get record :dependency)
              (cond
               ((plist-get record :invalid) "INVALID")
               (unresolved "BLOCKED")
               (t "READY"))
              (plist-get record :diagnostics)
              (sort (plist-get record :diagnostics)
                    (lambda (left right)
                      (string<
                       (format "%s\0%s"
                               (plist-get left :code)
                               (plist-get left :message))
                       (format "%s\0%s"
                               (plist-get right :code)
                               (plist-get right :message))))))))
    records))

(defun local-issues--records (root)
  "Return validated ticket records from ROOT in stable order."
  (local-issues--validate-and-resolve
   (sort (mapcar #'local-issues--parse-ticket
                 (local-issues--ticket-files root))
         (lambda (left right)
           (let ((left-key (cons (plist-get left :sort-id) (plist-get left :path)))
                 (right-key (cons (plist-get right :sort-id) (plist-get right :path))))
             (or (string< (car left-key) (car right-key))
                 (and (equal (car left-key) (car right-key))
                      (string< (cdr left-key) (cdr right-key)))))))))

(defun local-issues--closed-p (record)
  "Return non-nil when RECORD has a closed TODO state."
  (member (plist-get record :todo) local-issues--closed-states))

(defun local-issues--select-records (records all work-item)
  "Select RECORDS according to ALL and WORK-ITEM."
  (cl-remove-if-not
   (lambda (record)
     (and (or all (not (local-issues--closed-p record)))
          (or (null work-item)
              (equal work-item (plist-get record :work-item)))))
   records))

(defun local-issues--summary (records)
  "Return semantic summary metrics for selected RECORDS."
  (let ((open 0) (ready 0) (blocked 0) (active 0) (invalid 0))
    (dolist (record records)
      (unless (local-issues--closed-p record)
        (cl-incf open)
        (when (and (equal "READY-FOR-AGENT" (plist-get record :todo))
                   (equal "READY" (plist-get record :dependency))
                   (string-empty-p (plist-get record :assignee)))
          (cl-incf ready))
        (when (equal "BLOCKED" (plist-get record :dependency))
          (cl-incf blocked))
        (when (member (plist-get record :todo) '("IN-PROGRESS" "CLAIMED"))
          (cl-incf active))
        (when (equal "INVALID" (plist-get record :dependency))
          (cl-incf invalid))))
    (list :open open :ready ready :blocked blocked :active active :invalid invalid)))

(defun local-issues--summary-alist (summary)
  "Convert SUMMARY to a JSON-ready alist."
  `((open . ,(plist-get summary :open))
    (ready . ,(plist-get summary :ready))
    (blocked . ,(plist-get summary :blocked))
    (active . ,(plist-get summary :active))
    (invalid . ,(plist-get summary :invalid))))

(defun local-issues--diagnostic-alist (diagnostic)
  "Convert DIAGNOSTIC to a JSON-ready alist."
  `((code . ,(plist-get diagnostic :code))
    (message . ,(plist-get diagnostic :message))))

(defun local-issues--record-alist (record)
  "Convert RECORD to a semantic JSON-ready alist."
  `((id . ,(plist-get record :id))
    (todo . ,(plist-get record :todo))
    (dependency . ,(plist-get record :dependency))
    (blocked_by . ,(vconcat (plist-get record :unresolved-blockers)))
    (assignee . ,(plist-get record :assignee))
    (title . ,(plist-get record :title))
    (diagnostics . ,(vconcat
                     (mapcar #'local-issues--diagnostic-alist
                             (plist-get record :diagnostics))))))

(defun local-issues--print-summary (summary)
  "Print SUMMARY in stable table form."
  (princ
   (format "SUMMARY\topen=%d\tready=%d\tblocked=%d\tactive=%d\tinvalid=%d\n"
           (plist-get summary :open)
           (plist-get summary :ready)
           (plist-get summary :blocked)
           (plist-get summary :active)
           (plist-get summary :invalid))))

(defun local-issues--print-table (records summary)
  "Print RECORDS and SUMMARY as a stable plain-text table."
  (local-issues--print-summary summary)
  (princ "ID\tTODO\tDEPENDENCY\tBLOCKED_BY\tASSIGNEE\tTITLE\tDIAGNOSTICS\n")
  (dolist (record records)
    (princ
     (mapconcat
      #'identity
      (list (plist-get record :id)
            (plist-get record :todo)
            (plist-get record :dependency)
            (if-let* ((ids (plist-get record :unresolved-blockers)))
                (string-join ids ",")
              "-")
            (if (string-empty-p (plist-get record :assignee))
                "-"
              (plist-get record :assignee))
            (plist-get record :title)
            (if-let* ((diagnostics (plist-get record :diagnostics)))
                (string-join
                 (mapcar (lambda (diagnostic) (plist-get diagnostic :code)) diagnostics)
                 ",")
              "-"))
      "\t"))
    (princ "\n")))

(defun local-issues--print-json (records summary)
  "Print RECORDS and SUMMARY as semantic JSON."
  (princ
   (json-encode
    `((summary . ,(local-issues--summary-alist summary))
      (tickets . ,(vconcat (mapcar #'local-issues--record-alist records))))))
  (princ "\n"))

(defun local-issues--print-list (root options)
  "Print a ticket listing for ROOT according to OPTIONS."
  (let ((work-item (plist-get options :work-item)))
    (when (and work-item (not (member work-item (local-issues--work-items root))))
      (error "unknown work item %s" work-item))
    (let* ((records (local-issues--select-records
                     (local-issues--records root)
                     (plist-get options :all)
                     work-item))
           (summary (local-issues--summary records)))
      (pcase (plist-get options :format)
        ("table" (local-issues--print-table records summary))
        ("json" (local-issues--print-json records summary))))))

(defun local-issues--parse-arguments (arguments)
  "Parse launcher ARGUMENTS into a command plist."
  (let (command help root all work-item list-option)
    (let ((format "table"))
      (while arguments
        (pcase (pop arguments)
          ("--root"
           (unless arguments
             (error "--root requires a path"))
           (setq root (pop arguments)))
          ("--all" (setq all t list-option t))
          ("--work-item"
           (unless arguments
             (error "--work-item requires an ID"))
           (setq work-item (pop arguments)
                 list-option t)
           (unless (string-match-p "\\`[0-9]\\{3\\}\\'" work-item)
             (error "invalid work item %s" work-item)))
          ("--format"
           (unless arguments
             (error "--format requires table or json"))
           (setq format (pop arguments)
                 list-option t)
           (unless (member format '("table" "json"))
             (error "unknown format %s" format)))
          ("--help" (setq help t))
          ("list"
           (if command
               (error "unexpected command list")
             (setq command 'list)))
          (argument (error "unknown argument %s" argument))))
      (when (and (null command) list-option)
        (error "list options require the list command"))
      (list :command command
            :help help
            :root root
            :all all
            :work-item work-item
            :format format))))

(defun local-issues-cli-main ()
  "Dispatch `local-issues' using `command-line-args-left'."
  (let ((arguments (delete "--" command-line-args-left)))
    (setq command-line-args-left nil)
    (condition-case condition
        (let* ((options (local-issues--parse-arguments arguments))
               (command (plist-get options :command)))
          (cond
           ((and (eq command 'list) (plist-get options :help))
            (princ local-issues--list-help))
           ((or (null command) (plist-get options :help))
            (princ local-issues--help))
           ((eq command 'list)
            (local-issues--print-list
             (local-issues--repository-root (plist-get options :root))
             options))))
      (error
       (princ (format "local-issues: %s\n" (error-message-string condition))
              'external-debugging-output)
       (kill-emacs 2)))))

(provide 'local-issues-core)

;;; local-issues-core.el ends here
