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

(defvar local-issues-test--socket-name
  (format "local-issues-absent-%d" (emacs-pid))
  "Emacs server socket selected for launcher process tests.")

(defvar local-issues-test--server-counter 0)

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
        (process-environment (copy-sequence process-environment))
        (stdout (generate-new-buffer " *local-issues stdout*"))
        (stderr (make-temp-file "local-issues-stderr-")))
    (setenv "EMACS_SOCKET_NAME" local-issues-test--socket-name)
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

(defun local-issues-test--why-json-edges (document)
  (mapcar
   (lambda (edge)
     (list (alist-get 'from edge)
           (alist-get 'to edge)
           (alist-get 'depth edge)
           (eq t (alist-get 'reference edge))))
   (alist-get 'dependencies document)))

(defun local-issues-test--why-table-edges (result)
  (let* ((lines (split-string (plist-get result :stdout) "\n" t))
         (header "FROM\tTO\tDEPTH\tEXPANSION\tTODO\tDEPENDENCY\tBLOCKED_BY\tASSIGNEE\tTITLE\tDIAGNOSTICS"))
    (mapcar
     (lambda (line) (seq-take (split-string line "\t") 4))
     (cdr (member header lines)))))

(defun local-issues-test--doctor-json-rows (document)
  "Return doctor DOCUMENT diagnostics as stable row fields."
  (mapcar
   (lambda (diagnostic)
     (mapcar (lambda (key) (alist-get key diagnostic))
             '(severity code id source message)))
   (alist-get 'diagnostics document)))

(defun local-issues-test--doctor-table-rows (result)
  "Return doctor table RESULT as stable row fields."
  (cdr (mapcar (lambda (line) (split-string line "\t"))
               (split-string (plist-get result :stdout) "\n" t))))

(defun local-issues-test--file-snapshot (root)
  "Return the literal contents of every Org file below ROOT."
  (mapcar
   (lambda (path)
     (cons path
           (with-temp-buffer
             (insert-file-contents-literally path)
             (buffer-string))))
   (directory-files-recursively root "\\.org\\'")))

(cl-defmacro local-issues-test--with-repository ((root) &rest body)
  (declare (indent 1))
  `(let ((,root (make-temp-file "local-issues-repository-" t)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name ".scratch-org" ,root))
           ,@body)
       (delete-directory ,root t))))

(defun local-issues-test--server-eval (socket expression)
  "Evaluate EXPRESSION through the isolated Emacs server at SOCKET."
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "EMACS_SOCKET_NAME" socket)
    (call-process "emacsclient" nil nil nil
                  "--alternate-editor=false" "--timeout=2"
                  "--suppress-output" "--eval" expression)))

(cl-defmacro local-issues-test--with-server ((socket &optional load-core) &rest body)
  "Start an isolated server as SOCKET, optionally LOAD-CORE, then run BODY."
  (declare (indent 1))
  `(let ((,socket (format "local-issues-test-%d-%d"
                          (emacs-pid)
                          (cl-incf local-issues-test--server-counter))))
     (should (= 0 (call-process "emacs" nil nil nil
                                "-Q" (concat "--daemon=" ,socket))))
     (unwind-protect
         (progn
           (when ,load-core
             (should
              (= 0
                 (local-issues-test--server-eval
                  ,socket
                  (format "(load %S nil t)"
                          (expand-file-name
                           "configs/doom/lisp/local-issues-core.el"
                           local-issues-test--repository-root))))))
           ,@body)
       (local-issues-test--server-eval ,socket "(kill-emacs)"))))

(cl-defmacro local-issues-test--without-batch-emacs (&rest body)
  "Run BODY with an =emacs= command that fails with status 97."
  (declare (indent 0))
  `(let* ((bin (make-temp-file "local-issues-bin-" t))
          (emacs-shim (expand-file-name "emacs" bin))
          (process-environment (copy-sequence process-environment)))
     (unwind-protect
         (progn
           (with-temp-file emacs-shim
             (insert "#!/bin/sh\nexit 97\n"))
           (set-file-modes emacs-shim #o755)
           (setenv "PATH" (concat bin path-separator (getenv "PATH")))
           ,@body)
       (delete-directory bin t))))

(ert-deftest local-issues-uses-compatible-daemon-when-batch-is-unavailable ()
  (local-issues-test--with-repository (root)
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/01-first.org"
     "READY-FOR-AGENT" "001-01" "Daemon result")
    (local-issues-test--with-server (socket t)
      (let ((local-issues-test--socket-name socket))
        (local-issues-test--without-batch-emacs
          (let ((result (local-issues-test--run root "list")))
            (should
             (equal (list 0 t "")
                    (list (plist-get result :status)
                          (and (string-match-p "Daemon result"
                                               (plist-get result :stdout))
                               t)
                          (plist-get result :stderr))))))))))

(ert-deftest local-issues-falls-back-silently-for-unusable-daemons ()
  (local-issues-test--with-repository (root)
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/01-first.org"
     "READY-FOR-AGENT" "001-01" "Fallback result")
    (let ((fallback (local-issues-test--run root "list" "--format" "json")))
      (should (equal '(0 "")
                     (list (plist-get fallback :status)
                           (plist-get fallback :stderr))))
      (local-issues-test--with-server (socket)
        (let ((local-issues-test--socket-name socket))
          (should (equal fallback
                         (local-issues-test--run root "list" "--format" "json")))))
      (local-issues-test--with-server (socket)
        (should (= 0 (local-issues-test--server-eval
                      socket
                      "(setq local-issues-protocol-version \"1\")")))
        (let ((local-issues-test--socket-name socket))
          (should (equal fallback
                         (local-issues-test--run root "list" "--format" "json")))))
      (local-issues-test--with-server (socket t)
        (should (= 0 (local-issues-test--server-eval
                      socket
                      "(setq local-issues-protocol-version \"stale\")")))
        (let ((local-issues-test--socket-name socket))
          (should (equal fallback
                         (local-issues-test--run root "list" "--format" "json")))))
      (local-issues-test--with-server (socket t)
        (should
         (= 0
            (local-issues-test--server-eval
             socket
             (concat
              "(fset 'local-issues-daemon-request "
              "(lambda (_protocol _directory response-directory _arguments) "
              "(dolist (name '(\"stdout\" \"stderr\")) "
              "(with-temp-file (expand-file-name name response-directory))) "
              "(with-temp-file (expand-file-name \"response\" response-directory) "
              "(insert \"1\\n0\\n\")) t))"))))
        (let ((local-issues-test--socket-name socket))
          (should (equal fallback
                         (local-issues-test--run root "list" "--format" "json")))))
      (let ((transport (make-temp-file "local-issues-not-a-socket-")))
        (unwind-protect
            (let ((local-issues-test--socket-name transport))
              (should (equal fallback
                             (local-issues-test--run root "list" "--format" "json"))))
          (delete-file transport))))))

(ert-deftest local-issues-daemon-and-batch-have-full-command-parity ()
  (local-issues-test--with-repository (root)
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/01-resolved.org"
     "RESOLVED" "001-01" "Resolved")
    (local-issues-test--write
     root ".scratch-org/001-alpha/issues/02-candidate.org"
     "* READY-FOR-AGENT Candidate\n:PROPERTIES:\n:TICKET_ID: 001-02\n:BLOCKED_BY: 001-01\n:END:\n")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/03-dependent.org"
     "READY-FOR-AGENT" "001-03" "Dependent" "001-02")
    (let ((commands '(("list")
                      ("list" "--format" "json")
                      ("suggest")
                      ("suggest" "--format" "json")
                      ("why" "001-03")
                      ("why" "001-03" "--format" "json")
                      ("doctor")
                      ("doctor" "--format" "json")
                      ("why" "999-99"))))
      (local-issues-test--with-server (socket t)
        (dolist (arguments commands)
          (let ((batch (apply #'local-issues-test--run root arguments))
                (daemon
                 (let ((local-issues-test--socket-name socket))
                   (apply #'local-issues-test--run root arguments))))
            (should (equal batch daemon))))))))

(ert-deftest local-issues-daemon-reads-disk-instead-of-modified-buffer ()
  (local-issues-test--with-repository (root)
    (let ((path (local-issues-test--ticket
                 root ".scratch-org/001-alpha/issues/01-first.org"
                 "READY-FOR-AGENT" "001-01" "Saved title")))
      (local-issues-test--with-server (socket t)
        (should
         (= 0
            (local-issues-test--server-eval
             socket
             (format
              "(progn (find-file %S) (goto-char (point-min)) (search-forward \"Saved title\") (replace-match \"Unsaved title\") (set-buffer-modified-p t))"
              path))))
        (let ((local-issues-test--socket-name socket))
          (local-issues-test--without-batch-emacs
            (let ((result (local-issues-test--run root "list")))
              (should
               (equal '(0 t nil "")
                      (list (plist-get result :status)
                            (and (string-match-p "Saved title"
                                                 (plist-get result :stdout))
                                 t)
                            (string-match-p "Unsaved title"
                                            (plist-get result :stdout))
                            (plist-get result :stderr)))))))
        (local-issues-test--server-eval
         socket
         "(progn (set-buffer-modified-p nil) (kill-buffer))")))))

(ert-deftest local-issues-help-is-available ()
  (local-issues-test--with-repository (root)
    (dolist (arguments '(() ("--help") ("list" "--help")
                         ("suggest" "--help") ("why" "--help")
                         ("doctor" "--help")))
      (let ((result (apply #'local-issues-test--run root arguments)))
        (should (= 0 (plist-get result :status)))
        (should (string-match-p "Usage: local-issues" (plist-get result :stdout)))
        (should (string-empty-p (plist-get result :stderr))))))
  (local-issues-test--with-repository (root)
    (let ((help (local-issues-test--run root "why" "--help")))
      (dolist (text '("TICKET_ID" "--all" "--format" "Missing" "ambiguous" "invalid"))
        (should (string-match-p text (plist-get help :stdout))))))
  (local-issues-test--with-repository (root)
    (let ((help (local-issues-test--run root "suggest" "--help")))
      (dolist (text '("READY-FOR-AGENT" "READY" "Unassigned" "requiring"
                      "default: 1" "--limit" "--work-item" "--format"))
        (should (string-match-p text (plist-get help :stdout))))))
  (local-issues-test--with-repository (root)
    (let ((help (local-issues-test--run root "doctor" "--help")))
      (dolist (text '("read-only" "never edits" "--format" "zero" "nonzero"))
        (should (string-match-p text (plist-get help :stdout)))))))

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

(ert-deftest local-issues-why-explains-complete-graph-in-table-and-json ()
  (local-issues-test--with-repository (root)
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/01-branch-a.org"
     "READY-FOR-HUMAN" "001-01" "Branch A" "002-01")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/02-branch-b.org"
     "READY-FOR-HUMAN" "001-02" "Branch B" "002-01 001-03")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/03-wontfix.org"
     "WONTFIX" "001-03" "Wontfix blocker")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/04-resolved.org"
     "RESOLVED" "001-04" "Resolved blocker")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/06-linear.org"
     "READY-FOR-HUMAN" "001-06" "Linear branch" "001-07")
    (local-issues-test--write
     root ".scratch-org/001-alpha/issues/07-leaf.org"
     "* READY-FOR-HUMAN Diagnostic leaf\n:PROPERTIES:\n:TICKET_ID: 001-07\n:BLOCKED_BY:\n:END:\n")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/09-root.org"
     "READY-FOR-HUMAN" "001-09" "Root ticket" "001-01 001-02 001-06" "agent"
     "SECRET-TICKET-BODY")
    (local-issues-test--ticket
     root ".scratch-org/002-beta/issues/01-shared.org"
     "READY-FOR-HUMAN" "002-01" "Shared cross-item" "002-02 001-04")
    (local-issues-test--ticket
     root ".scratch-org/002-beta/issues/02-leaf.org"
     "READY-FOR-HUMAN" "002-02" "Cross-item leaf")
    (let* ((table (local-issues-test--run root "why" "001-09" "--format" "table"))
           (json-result (local-issues-test--run root "why" "001-09" "--format" "json"))
           (all-table (local-issues-test--run root "why" "001-09" "--all"))
           (all-json-result
            (local-issues-test--run root "why" "001-09" "--all" "--format" "json"))
           (document (local-issues-test--json json-result))
           (all-document (local-issues-test--json all-json-result))
           (expected-json
            '(("001-09" "001-01" 1 nil)
              ("001-01" "002-01" 2 nil)
              ("002-01" "002-02" 3 nil)
              ("001-09" "001-02" 1 nil)
              ("001-02" "001-03" 2 nil)
              ("001-02" "002-01" 2 t)
              ("001-09" "001-06" 1 nil)
              ("001-06" "001-07" 2 nil)))
           (expected-table
            '(("001-09" "001-01" "1" "EXPANDED")
              ("001-01" "002-01" "2" "EXPANDED")
              ("002-01" "002-02" "3" "EXPANDED")
              ("001-09" "001-02" "1" "EXPANDED")
              ("001-02" "001-03" "2" "EXPANDED")
              ("001-02" "002-01" "2" "REFERENCE")
              ("001-09" "001-06" "1" "EXPANDED")
              ("001-06" "001-07" "2" "EXPANDED")))
           (expected-all-json
            '(("001-09" "001-01" 1 nil)
              ("001-01" "002-01" 2 nil)
              ("002-01" "001-04" 3 nil)
              ("002-01" "002-02" 3 nil)
              ("001-09" "001-02" 1 nil)
              ("001-02" "001-03" 2 nil)
              ("001-02" "002-01" 2 t)
              ("001-09" "001-06" 1 nil)
              ("001-06" "001-07" 2 nil)))
           (expected-all-table
            (mapcar (lambda (edge)
                      (list (nth 0 edge) (nth 1 edge)
                            (number-to-string (nth 2 edge))
                            (if (nth 3 edge) "REFERENCE" "EXPANDED")))
                    expected-all-json)))
      (dolist (result (list table json-result all-table all-json-result))
        (should (= 0 (plist-get result :status)))
        (should (string-empty-p (plist-get result :stderr)))
        (should-not (string-match-p "SECRET-TICKET-BODY" (plist-get result :stdout))))
      (should (equal "001-09" (alist-get 'id (alist-get 'root document))))
      (should (equal "BLOCKED" (alist-get 'dependency (alist-get 'root document))))
      (should (equal '("001-01" "001-02" "001-06")
                     (alist-get 'blocked_by (alist-get 'root document))))
      (should (equal expected-json (local-issues-test--why-json-edges document)))
      (should (equal expected-table (local-issues-test--why-table-edges table)))
      (should (equal expected-all-json
                     (local-issues-test--why-json-edges all-document)))
      (should (equal expected-all-table
                     (local-issues-test--why-table-edges all-table)))
      (should-not (string-match-p "001-04" (plist-get table :stdout)))
      (should (string-match-p "001-03.*WONTFIX" (plist-get table :stdout)))
      (should (string-match-p "001-04.*RESOLVED" (plist-get all-table :stdout)))
      (should (string-match-p "001-07.*missing-assignee" (plist-get table :stdout)))
      (let* ((edges (alist-get 'dependencies document))
             (reference (nth 5 edges))
             (diagnostic-leaf (nth 7 edges)))
        (should (eq t (alist-get 'reference reference)))
        (should-not (alist-get 'ticket reference))
        (should (equal '("missing-assignee")
                       (local-issues-test--diagnostic-codes
                        (alist-get 'ticket diagnostic-leaf))))))))

(ert-deftest local-issues-why-fails-for-missing-ambiguous-and-invalid-tickets ()
  (local-issues-test--with-repository (root)
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/01-duplicate-a.org"
     "READY-FOR-AGENT" "001-02" "Duplicate A")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/02-duplicate-b.org"
     "READY-FOR-AGENT" "001-02" "Duplicate B")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/03-invalid.org"
     "READY-FOR-AGENT" "001-03" "Invalid" "999-99")
    (dolist (case '(("missing ticket 001-99" "why" "001-99")
                    ("ambiguous ticket 001-02" "why" "001-02")
                    ("invalid ticket 001-03" "why" "001-03")
                    ("why requires TICKET_ID" "why")
                    ("invalid ticket ID bad" "why" "bad")
                    ("--work-item is supported only by list"
                     "why" "001-03" "--work-item" "001")))
      (let ((result (apply #'local-issues-test--run root (cdr case))))
        (should-not (= 0 (plist-get result :status)))
        (should (string-empty-p (plist-get result :stdout)))
        (should (string-match-p (car case) (plist-get result :stderr)))))))

(ert-deftest local-issues-suggest-ranks-eligible-tickets-and-matches-json ()
  (local-issues-test--with-repository (root)
    (local-issues-test--write
     root ".scratch-org/001-alpha/issues/01-foundation.org"
     "* READY-FOR-AGENT Foundation A :bug:\n:PROPERTIES:\n:TICKET_ID: 001-01\n:BLOCKED_BY:\n:ASSIGNEE:\n:END:\n\nSECRET-SUGGESTION-BODY\n")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/02-alternative.org"
     "READY-FOR-AGENT" "001-02" "Alternative B")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/03-wrong-state.org"
     "READY-FOR-HUMAN" "001-03" "Human only")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/04-assigned.org"
     "READY-FOR-AGENT" "001-04" "Assigned" "" "worker")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/05-blocked.org"
     "READY-FOR-AGENT" "001-05" "Blocked" "001-20")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/06-invalid.org"
     "READY-FOR-AGENT" "001-06" "Invalid" "999-99")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/07-invalid-dependent.org"
     "READY-FOR-AGENT" "001-07" "Invalid dependent" "001-06")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/10-dependent-a.org"
     "READY-FOR-HUMAN" "001-10" "Requires A" "001-01")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/20-wontfix.org"
     "WONTFIX" "001-20" "Wontfix blocker")
    (local-issues-test--write
     root ".scratch-org/002-beta/issues/01-scoped.org"
     "* READY-FOR-AGENT Scoped C :bug:\n:PROPERTIES:\n:TICKET_ID: 002-01\n:BLOCKED_BY:\n:ASSIGNEE:\n:END:\n")
    (local-issues-test--ticket
     root ".scratch-org/002-beta/issues/10-dependent-all.org"
     "READY-FOR-HUMAN" "002-10" "Requires all" "001-01 001-02 002-01")
    (local-issues-test--ticket
     root ".scratch-org/002-beta/issues/11-dependent-all.org"
     "READY-FOR-HUMAN" "002-11" "Also requires all" "001-01 001-02 002-01")
    (local-issues-test--ticket
     root ".scratch-org/002-beta/issues/12-resolved-dependent.org"
     "RESOLVED" "002-12" "Closed dependent" "001-01")
    (local-issues-test--ticket
     root ".scratch-org/002-beta/issues/13-invalid-dependent.org"
     "READY-FOR-HUMAN" "002-13" "Invalid dependent" "001-01 999-99")
    (local-issues-test--ticket
     root ".scratch-org/003-empty/issues/01-human.org"
     "READY-FOR-HUMAN" "003-01" "No agent candidate")
    (let* ((default (local-issues-test--run root "suggest"))
           (limited-table (local-issues-test--run root "suggest" "--limit" "3"))
           (limited-json-result
            (local-issues-test--run root "suggest" "--limit" "99" "--format" "json"))
           (scoped-result
            (local-issues-test--run root "suggest" "--work-item" "002" "--format" "json"))
           (empty-result
            (local-issues-test--run root "suggest" "--work-item" "003" "--format" "json"))
           (suggestions (alist-get 'suggestions
                                   (local-issues-test--json limited-json-result)))
           (ids (mapcar (lambda (suggestion) (alist-get 'id suggestion)) suggestions)))
      (dolist (result (list default limited-table limited-json-result scoped-result empty-result))
        (should (= 0 (plist-get result :status)))
        (should (string-empty-p (plist-get result :stderr)))
        (should-not (string-match-p "SECRET-SUGGESTION-BODY"
                                    (plist-get result :stdout))))
      (should (equal '("001-01" "001-02" "002-01") ids))
      (should (equal '(3 2 2)
                     (mapcar (lambda (suggestion) (alist-get 'impact suggestion))
                             suggestions)))
      (should (equal '("required by 3 open tickets"
                       "required by 2 open tickets"
                       "required by 2 open tickets")
                     (mapcar (lambda (suggestion) (alist-get 'reason suggestion))
                             suggestions)))
      (should (equal (mapcar (lambda (id)
                               (expand-file-name
                                (pcase id
                                  ("001-01" ".scratch-org/001-alpha/issues/01-foundation.org")
                                  ("001-02" ".scratch-org/001-alpha/issues/02-alternative.org")
                                  ("002-01" ".scratch-org/002-beta/issues/01-scoped.org"))
                                root))
                             ids)
                     (mapcar (lambda (suggestion) (alist-get 'path suggestion))
                             suggestions)))
      (should (equal '("001-01")
                     (mapcar (lambda (line) (car (split-string line "\t")))
                             (cdr (split-string (plist-get default :stdout) "\n" t)))))
      (dolist (suggestion suggestions)
        (should (string-match-p
                 (regexp-quote
                  (format "%s\t%s\t%s\t%s\t%d\t%s"
                          (alist-get 'id suggestion)
                          (alist-get 'todo suggestion)
                          (alist-get 'title suggestion)
                          (alist-get 'reason suggestion)
                          (alist-get 'impact suggestion)
                          (alist-get 'path suggestion)))
                 (plist-get limited-table :stdout))))
      (should (equal '("002-01")
                     (mapcar (lambda (suggestion) (alist-get 'id suggestion))
                             (alist-get 'suggestions
                                        (local-issues-test--json scoped-result)))))
      (should (equal '((suggestions))
                     (local-issues-test--json empty-result))))))

(ert-deftest local-issues-doctor-reports-every-finding-deterministically ()
  (local-issues-test--with-repository (root)
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/01-unknown-blocker.org"
     "READY-FOR-AGENT" "001-01" "Unknown blocker" "999-99" nil
     "SECRET-DOCTOR-BODY")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/02-malformed-blocker.org"
     "READY-FOR-AGENT" "001-02" "Malformed blocker" "bad")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/03-malformed-id.org"
     "READY-FOR-AGENT" "bad-id" "Malformed ID")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/04-mismatch.org"
     "READY-FOR-AGENT" "900-04" "Path mismatch")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/05-unknown-todo.org"
     "BOGUS" "001-05" "Unknown TODO")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/06-duplicate-a.org"
     "READY-FOR-AGENT" "001-06" "Duplicate A")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/07-duplicate-b.org"
     "READY-FOR-AGENT" "001-06" "Duplicate B")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/08-ambiguous.org"
     "READY-FOR-AGENT" "001-08" "Ambiguous blocker" "001-06")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/09-cycle-a.org"
     "READY-FOR-AGENT" "001-09" "Cycle A" "001-10")
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/10-cycle-b.org"
     "READY-FOR-AGENT" "001-10" "Cycle B" "001-09")
    (local-issues-test--write
     root ".scratch-org/001-alpha/issues/11-missing-id.org"
     "* READY-FOR-AGENT Missing ID\n:PROPERTIES:\n:BLOCKED_BY:\n:ASSIGNEE:\n:END:\n")
    (local-issues-test--write
     root ".scratch-org/001-alpha/issues/12-missing-assignee.org"
     "* READY-FOR-AGENT Missing assignee\n:PROPERTIES:\n:TICKET_ID: 001-12\n:BLOCKED_BY:\n:END:\n")
    (local-issues-test--write
     root ".scratch-org/001-alpha/issues/13-missing-blocked-by.org"
     "* READY-FOR-AGENT Missing blockers property\n:PROPERTIES:\n:TICKET_ID: 001-13\n:ASSIGNEE:\n:END:\n")
    (let* ((before (local-issues-test--file-snapshot root))
           (table (local-issues-test--run root "doctor" "--format" "table"))
           (json-result (local-issues-test--run root "doctor" "--format" "json"))
           (repeat (local-issues-test--run root "doctor" "--format" "json"))
           (document (local-issues-test--json json-result))
           (rows (local-issues-test--doctor-json-rows document))
           (codes (delete-dups (mapcar #'cadr rows))))
      (dolist (result (list table json-result repeat))
        (should-not (= 0 (plist-get result :status)))
        (should (string-empty-p (plist-get result :stderr)))
        (should-not (string-match-p "SECRET-DOCTOR-BODY"
                                    (plist-get result :stdout))))
      (should (equal (plist-get json-result :stdout)
                     (plist-get repeat :stdout)))
      (should (equal rows (local-issues-test--doctor-table-rows table)))
      (should (equal '("ambiguous-blocker" "dependency-cycle" "duplicate-id"
                       "malformed-blocker" "malformed-id" "missing-assignee"
                       "missing-blocked-by"
                       "missing-id" "path-id-mismatch" "unknown-blocker"
                       "unknown-todo")
                     (sort (cl-intersection
                            codes
                            '("ambiguous-blocker" "dependency-cycle" "duplicate-id"
                              "malformed-blocker" "malformed-id" "missing-assignee"
                              "missing-blocked-by"
                              "missing-id" "path-id-mismatch" "unknown-blocker"
                              "unknown-todo")
                            :test #'equal)
                           #'string<)))
      (should (cl-every (lambda (row)
                          (and (member (car row) '("error" "warning"))
                               (string-match-p "\\`[0-9]\\{3\\}-[0-9]\\{2\\}\\'"
                                               (nth 2 row))
                               (file-name-absolute-p (nth 3 row))
                               (not (string-empty-p (nth 4 row)))))
                        rows))
      (should (equal '("missing-assignee" "missing-blocked-by")
                     (sort (mapcar #'cadr
                                   (cl-remove-if-not
                                    (lambda (row) (equal "warning" (car row)))
                                    rows))
                           #'string<)))
      (should (equal before (local-issues-test--file-snapshot root))))))

(ert-deftest local-issues-doctor-clean-tracker-exits-zero ()
  (local-issues-test--with-repository (root)
    (local-issues-test--ticket
     root ".scratch-org/001-alpha/issues/01-clean.org"
     "READY-FOR-AGENT" "001-01" "Clean")
    (let ((table (local-issues-test--run root "doctor"))
          (json-result (local-issues-test--run root "doctor" "--format" "json")))
      (dolist (result (list table json-result))
        (should (= 0 (plist-get result :status)))
        (should (string-empty-p (plist-get result :stderr))))
      (should (equal "SEVERITY\tCODE\tID\tSOURCE\tMESSAGE\n"
                     (plist-get table :stdout)))
      (should (equal '((diagnostics))
                     (local-issues-test--json json-result))))))

(ert-deftest local-issues-operational-and-argument-errors-are-nonzero ()
  (local-issues-test--with-repository (root)
    (let ((path (local-issues-test--ticket
                 root ".scratch-org/001-alpha/issues/01-ticket.org"
                 "READY-FOR-AGENT" "001-01" "Ticket")))
      (dolist (arguments '(("list" "--format" "yaml")
                           ("list" "--work-item" "abc")
                           ("list" "--wat")
                           ("suggest" "--limit" "0")
                           ("suggest" "--limit" "nope")
                           ("suggest" "--all")
                           ("suggest" "--work-item" "999")
                           ("why" "001-01" "--limit" "2")
                           ("doctor" "--fix")
                           ("doctor" "--work-item" "001")
                           ("--all")
                           ("--limit" "2")
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
