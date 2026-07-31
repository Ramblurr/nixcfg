;;; local-issues-test.el --- Process tests for local-issues -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'subr-x)

(defconst local-issues-test--repository-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defconst local-issues-test--launcher
  (or (getenv "LOCAL_ISSUES_LAUNCHER")
      (expand-file-name "configs/doom/bin/local-issues"
                        local-issues-test--repository-root)))

(defun local-issues-test--write (root relative content)
  (let ((path (expand-file-name relative root)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert content))
    path))

(defun local-issues-test--ticket (root relative state id title &optional blockers assignee body)
  (local-issues-test--write
   root relative
   (format "* %s %s :enhancement:\n:PROPERTIES:\n:TICKET_ID: %s\n:BLOCKED_BY: %s\n:ASSIGNEE: %s\n:END:\n\n%s\n"
           state title id (or blockers "") (or assignee "") (or body "** What to build\nBody"))))

(defun local-issues-test--run (directory &rest arguments)
  (let ((default-directory (file-name-as-directory directory))
        (stdout (generate-new-buffer " *local-issues stdout*"))
        (stderr (make-temp-file "local-issues-stderr-")))
    (unwind-protect
        (let ((status (apply #'call-process
                             local-issues-test--launcher nil
                             (list stdout stderr) nil arguments)))
          (list :status status
                :stdout (with-current-buffer stdout (buffer-string))
                :stderr (with-temp-buffer
                          (insert-file-contents stderr)
                          (buffer-string))))
      (kill-buffer stdout)
      (delete-file stderr))))

(defun local-issues-test--json (result)
  (json-parse-string (plist-get result :stdout)
                     :object-type 'alist
                     :array-type 'list))

(defun local-issues-test--json-ticket (document id)
  (cl-find id (alist-get 'tickets document)
           :key (lambda (ticket) (alist-get 'id ticket))
           :test #'equal))

(defun local-issues-test--diagnostic-codes (ticket)
  (mapcar (lambda (diagnostic) (alist-get 'code diagnostic))
          (alist-get 'diagnostics ticket)))

(cl-defmacro local-issues-test--with-repository ((root) &rest body)
  (declare (indent 1))
  `(let ((,root (make-temp-file "local-issues-repository-" t)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name ".scratch-org" ,root))
           ,@body)
       (delete-directory ,root t))))

(ert-deftest local-issues-help-is-available ()
  (local-issues-test--with-repository (root)
    (dolist (arguments '(() ("--help") ("list" "--help")))
      (let ((result (apply #'local-issues-test--run root arguments)))
        (should (= 0 (plist-get result :status)))
        (should (string-match-p "Usage: local-issues" (plist-get result :stdout)))
        (should (string-empty-p (plist-get result :stderr)))))))

(ert-deftest local-issues-discovers-root-and-accepts-override ()
  (local-issues-test--with-repository (root)
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/01-first.org"
     "READY-FOR-AGENT" "001-01" "First")
    (let* ((nested (expand-file-name "one/two" root))
           (_ (make-directory nested t))
           (discovered (local-issues-test--run nested "list"))
           (elsewhere (make-temp-file "local-issues-elsewhere-" t)))
      (unwind-protect
          (let ((overridden (local-issues-test--run elsewhere "--root" root "list")))
            (should (= 0 (plist-get discovered :status)))
            (should (equal (plist-get discovered :stdout)
                           (plist-get overridden :stdout))))
        (delete-directory elsewhere t)))))

(ert-deftest local-issues-scans-only-canonical-ticket-files ()
  (local-issues-test--with-repository (root)
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/01-real.org"
     "READY-FOR-AGENT" "001-01" "Real ticket")
    (dolist (relative '(".scratch-org/001-alpha/spec.org"
                        ".scratch-org/001-alpha/concept.org"
                        ".scratch-org/001-alpha/reports/01-report.org"
                        ".scratch-org/001-alpha/research/01-research.org"
                        ".scratch-org/001-alpha/issues/legacy.org"
                        ".scratch-org/incidents/001-note.org"))
      (local-issues-test--write root relative "* READY-FOR-AGENT Not a ticket\n"))
    (let ((output (plist-get (local-issues-test--run root "list") :stdout)))
      (should (string-match-p "001-01.*Real ticket" output))
      (should-not (string-match-p "Not a ticket" output)))))

(ert-deftest local-issues-reads-first-heading-metadata-from-disk ()
  (local-issues-test--with-repository (root)
    (let* ((path (local-issues-test--ticket
                  root ".scratch-org/001-alpha/issues/01-saved.org"
                  "READY-FOR-AGENT" "001-01" "Saved title" "" nil
                  "** Body\n*TICKET_ID: 999-99\n* READY-FOR-AGENT Body heading"))
           (buffer (find-file-noselect path)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "Saved title")
              (replace-match "Unsaved title"))
            (let ((output (plist-get (local-issues-test--run root "list") :stdout)))
              (should (string-match-p "Saved title" output))
              (should-not (string-match-p "Unsaved\|999-99\|Body heading" output))))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest local-issues-resolves-blockers-repository-wide ()
  (local-issues-test--with-repository (root)
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/01-done.org"
     "RESOLVED" "001-01" "Done")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/02-wontfix.org"
     "WONTFIX" "001-02" "Not done")
    (local-issues-test--ticket
     root ".scratch-org/002-beta/issues/01-ready.org"
     "READY-FOR-AGENT" "002-01" "Cross-item ready" "001-01")
    (local-issues-test--ticket
     root ".scratch-org/002-beta/issues/02-blocked.org"
     "READY-FOR-AGENT" "002-02" "Wontfix blocked" "001-02")
    (let ((output (plist-get (local-issues-test--run root "list") :stdout)))
      (should (string-match-p "002-01\tREADY-FOR-AGENT\tREADY\t-\t-\tCross-item ready" output))
      (should (string-match-p "002-02\tREADY-FOR-AGENT\tBLOCKED\t001-02\t-\tWontfix blocked" output))
      (should-not (string-match-p "001-01\tRESOLVED\|001-02\tWONTFIX" output)))))

(ert-deftest local-issues-list-rows-are-complete-and-sorted ()
  (local-issues-test--with-repository (root)
    (local-issues-test--ticket
     root ".scratch-org/010-zeta/issues/02-second.org"
     "CLAIMED" "010-02" "Second" "" "worker")
    (local-issues-test--ticket
     root ".scratch-org/002-alpha/issues/01-first.org"
     "READY-FOR-HUMAN" "002-01" "First")
    (let ((result (local-issues-test--run root "list")))
      (should (= 0 (plist-get result :status)))
      (should
       (equal
        "SUMMARY\topen=2\tready=0\tblocked=0\tactive=1\tinvalid=0\nID\tTODO\tDEPENDENCY\tBLOCKED_BY\tASSIGNEE\tTITLE\tDIAGNOSTICS\n002-01\tREADY-FOR-HUMAN\tREADY\t-\t-\tFirst\t-\n010-02\tCLAIMED\tREADY\t-\tworker\tSecond\t-\n"
        (plist-get result :stdout))))))

(ert-deftest local-issues-list-supports-all-and-work-item-scoping ()
  (local-issues-test--with-repository (root)
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/01-done.org"
     "RESOLVED" "001-01" "Done")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/02-ready.org"
     "READY-FOR-AGENT" "001-02" "Ready")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/03-blocked.org"
     "READY-FOR-AGENT" "001-03" "Blocked" "002-01")
    (local-issues-test--ticket
     root ".scratch-org/002-beta/issues/01-wontfix.org"
     "WONTFIX" "002-01" "Wontfix")
    (local-issues-test--ticket
     root ".scratch-org/002-beta/issues/02-active.org"
     "IN-PROGRESS" "002-02" "Active" "" "worker")
    (let ((default (local-issues-test--run root "list"))
          (all (local-issues-test--run root "list" "--all"))
          (scoped (local-issues-test--run root "list" "--work-item" "001"))
          (unknown (local-issues-test--run root "list" "--work-item" "999")))
      (should (string-prefix-p
               "SUMMARY\topen=3\tready=1\tblocked=1\tactive=1\tinvalid=0\n"
               (plist-get default :stdout)))
      (should-not (string-match-p "\n001-01\t" (plist-get default :stdout)))
      (should-not (string-match-p "\n002-01\t" (plist-get default :stdout)))
      (should (string-match-p "001-01.*Done" (plist-get all :stdout)))
      (should (string-match-p "002-01.*Wontfix" (plist-get all :stdout)))
      (should (string-prefix-p
               "SUMMARY\topen=3\tready=1\tblocked=1\tactive=1\tinvalid=0\n"
               (plist-get all :stdout)))
      (should (string-prefix-p
               "SUMMARY\topen=2\tready=1\tblocked=1\tactive=0\tinvalid=0\n"
               (plist-get scoped :stdout)))
      (should-not (string-match-p "\n002-0[12]\t" (plist-get scoped :stdout)))
      (should-not (= 0 (plist-get unknown :status)))
      (should (string-match-p "unknown work item 999"
                              (plist-get unknown :stderr))))))

(ert-deftest local-issues-json-is-semantic-and-matches-table ()
  (local-issues-test--with-repository (root)
    (local-issues-test--write
     root ".scratch-org/001-alpha/issues/01-ready.org"
     "* READY-FOR-AGENT Ready\n:PROPERTIES:\n:TICKET_ID: 001-01\n:BLOCKED_BY:\n:END:\n")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/02-blocked.org"
     "READY-FOR-HUMAN" "001-02" "Blocked" "001-01" "worker")
    (let* ((table (local-issues-test--run root "list" "--format" "table"))
           (json-result (local-issues-test--run root "list" "--format" "json"))
           (document (local-issues-test--json json-result))
           (tickets (alist-get 'tickets document)))
      (should (= 0 (plist-get json-result :status)))
      (should (equal '((open . 2) (ready . 1) (blocked . 1)
                       (active . 0) (invalid . 0))
                     (alist-get 'summary document)))
      (should (equal '("001-01" "001-02")
                     (mapcar (lambda (ticket) (alist-get 'id ticket)) tickets)))
      (should (equal '("missing-assignee")
                     (local-issues-test--diagnostic-codes (car tickets))))
      (should (string-match-p
               "001-01\tREADY-FOR-AGENT\tREADY\t-\t-\tReady\tmissing-assignee"
               (plist-get table :stdout)))
      (should (equal '("001-01") (alist-get 'blocked_by (cadr tickets))))
      (should (equal "BLOCKED" (alist-get 'dependency (cadr tickets))))
      (should (string-match-p
               "001-02\tREADY-FOR-HUMAN\tBLOCKED\t001-01\tworker\tBlocked"
               (plist-get table :stdout)))
      (should-not (string-match-p "\\(?:\\e\\|\\033\\)\\["
                                  (plist-get json-result :stdout))))))

(ert-deftest local-issues-invalid-data-is-diagnostic-and-partial ()
  (local-issues-test--with-repository (root)
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/01-unknown.org"
     "READY-FOR-AGENT" "001-01" "Unknown blocker" "999-99")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/02-malformed-blocker.org"
     "READY-FOR-AGENT" "001-02" "Malformed blocker" "bad")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/03-malformed-id.org"
     "READY-FOR-AGENT" "bad-id" "Malformed ID")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/04-mismatch.org"
     "READY-FOR-AGENT" "002-09" "Mismatch")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/05-unknown-todo.org"
     "BOGUS" "001-05" "Unknown TODO")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/06-duplicate-a.org"
     "READY-FOR-AGENT" "001-07" "Duplicate A")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/07-duplicate-b.org"
     "READY-FOR-AGENT" "001-07" "Duplicate B")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/08-ambiguous.org"
     "READY-FOR-AGENT" "001-08" "Ambiguous" "001-07")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/09-cycle-a.org"
     "READY-FOR-AGENT" "001-09" "Cycle A" "001-10")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/10-cycle-b.org"
     "READY-FOR-AGENT" "001-10" "Cycle B" "001-09")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/11-dependent.org"
     "READY-FOR-AGENT" "001-11" "Dependent" "001-01")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/12-transitive.org"
     "READY-FOR-AGENT" "001-12" "Transitive" "001-11")
    (local-issues-test--ticket
     root ".scratch-org/002-beta/issues/01-valid.org"
     "READY-FOR-AGENT" "002-01" "Valid")
    (local-issues-test--write
     root ".scratch-org/002-beta/issues/02-unassigned.org"
     "* READY-FOR-AGENT Missing assignee\n:PROPERTIES:\n:TICKET_ID: 002-02\n:BLOCKED_BY:\n:END:\n")
    (let* ((result (local-issues-test--run root "list" "--format" "json"))
           (repeat (local-issues-test--run root "list" "--format" "json"))
           (table (local-issues-test--run root "list" "--format" "table"))
           (document (local-issues-test--json result))
           (summary (alist-get 'summary document)))
      (should (= 0 (plist-get result :status)))
      (should (equal (plist-get result :stdout) (plist-get repeat :stdout)))
      (should (equal 14 (alist-get 'open summary)))
      (should (equal 2 (alist-get 'ready summary)))
      (should (equal 12 (alist-get 'invalid summary)))
      (should (= 0 (plist-get table :status)))
      (should (string-match-p
               "001-01\tREADY-FOR-AGENT\tINVALID\t999-99.*unknown-blocker"
               (plist-get table :stdout)))
      (should (string-match-p
               "002-01\tREADY-FOR-AGENT\tREADY\t-\t-\tValid\t-"
               (plist-get table :stdout)))
      (dolist (case '(("001-01" "unknown-blocker")
                      ("001-02" "malformed-blocker")
                      ("bad-id" "malformed-id")
                      ("002-09" "path-id-mismatch")
                      ("001-05" "unknown-todo")
                      ("001-07" "duplicate-id")
                      ("001-08" "ambiguous-blocker")
                      ("001-09" "dependency-cycle")
                      ("001-11" "invalid-blocker")
                      ("001-12" "invalid-blocker")
                      ("002-02" "missing-assignee")))
        (should (member (cadr case)
                        (local-issues-test--diagnostic-codes
                         (local-issues-test--json-ticket document (car case))))))
      (should (equal "READY"
                     (alist-get 'dependency
                                (local-issues-test--json-ticket document "002-01"))))
      (should (equal "INVALID"
                     (alist-get 'dependency
                                (local-issues-test--json-ticket document "001-12")))))))

(ert-deftest local-issues-sorts-path-mismatches-by-displayed-canonical-id ()
  (local-issues-test--with-repository (root)
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/01-first.org"
     "READY-FOR-AGENT" "001-01" "First")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/02-mismatch.org"
     "READY-FOR-AGENT" "900-01" "Mismatch")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/03-third.org"
     "READY-FOR-AGENT" "001-03" "Third")
    (let* ((result (local-issues-test--run root "list" "--format" "json"))
           (tickets (alist-get 'tickets (local-issues-test--json result))))
      (should (= 0 (plist-get result :status)))
      (should (equal '("001-01" "001-03" "900-01")
                     (mapcar (lambda (ticket) (alist-get 'id ticket)) tickets)))
      (should (equal '("path-id-mismatch")
                     (local-issues-test--diagnostic-codes (car (last tickets))))))))

(ert-deftest local-issues-operational-and-argument-errors-are-nonzero ()
  (local-issues-test--with-repository (root)
    (let ((path (local-issues-test--ticket
                 root ".scratch-org/001-alpha/issues/01-ticket.org"
                 "READY-FOR-AGENT" "001-01" "Ticket")))
      (dolist (arguments '(("list" "--format" "yaml")
                           ("list" "--work-item" "abc")
                           ("list" "--wat")
                           ("--all")
                           ("--format" "json")
                           ("--work-item" "001")))
        (let ((result (apply #'local-issues-test--run root arguments)))
          (should-not (= 0 (plist-get result :status)))
          (should (string-empty-p (plist-get result :stdout)))
          (should-not (string-empty-p (plist-get result :stderr)))))
      (unwind-protect
          (progn
            (set-file-modes path 0)
            (let ((result (local-issues-test--run root "list")))
              (should-not (= 0 (plist-get result :status)))
              (should (string-empty-p (plist-get result :stdout)))
              (should-not (string-empty-p (plist-get result :stderr)))))
        (set-file-modes path #o600))))
  (let ((directory (make-temp-file "local-issues-no-root-" t)))
    (unwind-protect
        (let ((result (local-issues-test--run directory "list")))
          (should-not (= 0 (plist-get result :status)))
          (should (string-match-p "no .scratch-org directory"
                                  (plist-get result :stderr))))
      (delete-directory directory t))))

;;; local-issues-test.el ends here
