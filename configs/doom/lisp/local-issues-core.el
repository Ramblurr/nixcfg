;;; local-issues-core.el --- Read project-local Org issue metadata -*- lexical-binding: t; -*-

(require 'cl-lib)
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
  "Usage: local-issues [--root PATH] COMMAND\n\nCommands:\n  list       List open canonical tickets\n\nOptions:\n  --root PATH  Use PATH instead of discovering the tracker root\n  --help       Show this help\n")

(defconst local-issues--list-help
  "Usage: local-issues [--root PATH] list\n\nList open canonical tickets sorted by ticket ID.\n")

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

(defun local-issues--parse-ticket (path)
  "Read PATH from disk and return its first top-level heading metadata."
  (with-temp-buffer
    (insert-file-contents path)
    (delay-mode-hooks (org-mode))
    (goto-char (point-min))
    (unless (re-search-forward "^\\* " nil t)
      (error "missing top-level heading in %s" path))
    (beginning-of-line)
    (let* ((heading-parts
            (split-string (org-get-heading t t t t) "[[:space:]]+" t))
           (state (car heading-parts))
           (title (string-join (cdr heading-parts) " "))
           (id (org-entry-get nil "TICKET_ID"))
           (expected-id (local-issues--expected-id path))
           (blocker-value (org-entry-get nil "BLOCKED_BY"))
           (assignee (string-trim (or (org-entry-get nil "ASSIGNEE") ""))))
      (unless (member state local-issues--todo-states)
        (error "missing or unknown TODO state in %s" path))
      (unless (and id (string-match-p "\\`[0-9]\\{3\\}-[0-9]\\{2\\}\\'" id))
        (error "missing or malformed TICKET_ID in %s" path))
      (unless (equal id expected-id)
        (error "ticket ID %s does not match path ID %s" id expected-id))
      (let ((blockers (split-string (or blocker-value "") "[[:space:]]+" t)))
        (dolist (blocker blockers)
          (unless (string-match-p "\\`[0-9]\\{3\\}-[0-9]\\{2\\}\\'" blocker)
            (error "malformed blocker %s on %s" blocker id)))
        (list :id id
              :path path
              :work-item (substring id 0 3)
              :title title
              :todo state
              :assignee assignee
              :blockers blockers
              :unresolved-blockers nil
              :dependency nil)))))

(defun local-issues--validate-and-resolve (records)
  "Validate dependency relationships in RECORDS and add derived fields."
  (let ((by-id (make-hash-table :test #'equal))
        (visits (make-hash-table :test #'equal)))
    (dolist (record records)
      (let ((id (plist-get record :id)))
        (when (gethash id by-id)
          (error "duplicate ticket ID %s" id))
        (puthash id record by-id)))
    (dolist (record records)
      (dolist (blocker (plist-get record :blockers))
        (unless (gethash blocker by-id)
          (error "unknown blocker %s referenced by %s"
                 blocker (plist-get record :id)))))
    (cl-labels ((visit
                 (id)
                 (pcase (gethash id visits)
                   ('done nil)
                   ('visiting
                    (error "dependency cycle involving %s" id))
                   (_
                    (puthash id 'visiting visits)
                    (dolist (blocker (plist-get (gethash id by-id) :blockers))
                      (visit blocker))
                    (puthash id 'done visits)))))
      (maphash (lambda (id _record) (visit id)) by-id))
    (dolist (record records)
      (let ((unresolved
             (cl-remove-if
              (lambda (id)
                (equal "RESOLVED" (plist-get (gethash id by-id) :todo)))
              (plist-get record :blockers))))
        (setf (plist-get record :unresolved-blockers) unresolved
              (plist-get record :dependency)
              (if unresolved "BLOCKED" "READY"))))
    records))

(defun local-issues--records (root)
  "Return validated ticket records from ROOT in canonical ID order."
  (local-issues--validate-and-resolve
   (sort (mapcar #'local-issues--parse-ticket
                 (local-issues--ticket-files root))
         (lambda (left right)
           (string< (plist-get left :id) (plist-get right :id))))))

(defun local-issues--print-list (root)
  "Print the default open-ticket table for ROOT."
  (let ((records (local-issues--records root)))
    (princ "ID\tTODO\tDEPENDENCY\tBLOCKED_BY\tASSIGNEE\tTITLE\n")
    (dolist (record records)
      (unless (member (plist-get record :todo) local-issues--closed-states)
        (princ
         (mapconcat
          #'identity
          (list (plist-get record :id)
                (plist-get record :todo)
                (plist-get record :dependency)
                (or (and-let* ((ids (plist-get record :unresolved-blockers)))
                      (string-join ids ","))
                    "-")
                (if (string-empty-p (plist-get record :assignee))
                    "-"
                  (plist-get record :assignee))
                (plist-get record :title))
          "\t"))
        (princ "\n")))))

(defun local-issues--parse-arguments (arguments)
  "Parse launcher ARGUMENTS into a command plist."
  (let (command help root)
    (while arguments
      (pcase (pop arguments)
        ("--root"
         (unless arguments
           (error "--root requires a path"))
         (setq root (pop arguments)))
        ("--help" (setq help t))
        ("list"
         (if command
             (error "unexpected command list")
           (setq command 'list)))
        (argument (error "unknown argument %s" argument))))
    (list :command command :help help :root root)))

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
             (local-issues--repository-root (plist-get options :root))))))
      (error
       (princ (format "local-issues: %s\n" (error-message-string condition))
              'external-debugging-output)
       (kill-emacs 2)))))

(provide 'local-issues-core)

;;; local-issues-core.el ends here
