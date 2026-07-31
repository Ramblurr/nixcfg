;;; local-issues-test.el --- Process tests for local-issues -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
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
        "ID\tTODO\tDEPENDENCY\tBLOCKED_BY\tASSIGNEE\tTITLE\n002-01\tREADY-FOR-HUMAN\tREADY\t-\t-\tFirst\n010-02\tCLAIMED\tREADY\t-\tworker\tSecond\n"
        (plist-get result :stdout))))))

(ert-deftest local-issues-malformed-tracker-fails-clearly ()
  (local-issues-test--with-repository (root)
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/01-broken.org"
     "READY-FOR-AGENT" "001-01" "Broken" "999-99")
    (let ((result (local-issues-test--run root "list")))
      (should-not (= 0 (plist-get result :status)))
      (should (string-empty-p (plist-get result :stdout)))
      (should (string-match-p "unknown blocker 999-99.*001-01"
                              (plist-get result :stderr))))))

;;; local-issues-test.el ends here
