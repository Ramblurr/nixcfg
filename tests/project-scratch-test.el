;;; project-scratch-test.el --- Project scratch command tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'org-agenda)
(require 'project)
(require 'subr-x)

(unless (fboundp 'after!)
  (defmacro after! (_feature &rest body)
    `(progn ,@body)))

(unless (fboundp 'map!)
  (defmacro map! (&rest _arguments)))

(load
 (expand-file-name
  "../configs/doom/+local-issues.el"
  (file-name-directory (or load-file-name buffer-file-name)))
 nil
 t)

(defun project-scratch-test--write (root relative-path contents)
  (let ((path (expand-file-name relative-path root)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert contents))
    path))

(defun project-scratch-test--closed-ids ()
  (let (ids)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^Closed$")
      (while (re-search-forward "^ \\([0-9]\\{3\\}-[0-9]\\{2\\}\\)" nil t)
        (push (match-string 1) ids)))
    (nreverse ids)))

(defun project-scratch-test--load-more-closed-button ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "older closed tickets remain" nil t)
      (button-at (match-beginning 0)))))

(defun project-scratch-test--completions (input collection)
  (let ((completion-styles '(substring)))
    (let ((completions
           (completion-all-completions input collection nil (length input)))
          result)
      (while (consp completions)
        (push (substring-no-properties (pop completions)) result))
      (nreverse result))))

(cl-defmacro project-scratch-test--with-project ((root) &rest body)
  (declare (indent 1))
  `(let* ((,root (file-name-as-directory
                  (make-temp-file "project-scratch-test-" t)))
          (default-directory ,root)
          (project-find-functions
           (list (lambda (_directory) (cons 'transient ,root)))))
     (unwind-protect
         (progn ,@body)
       (dolist (buffer (buffer-list))
         (let ((file (buffer-file-name buffer)))
           (when (or (equal (buffer-name buffer) org-agenda-buffer-name)
                     (and file (file-in-directory-p file ,root)))
             (kill-buffer buffer))))
       (delete-directory ,root t))))

(ert-deftest my-project-scratch-agenda-opens-current-project ()
  (project-scratch-test--with-project (root)
    (let ((work-item
           (expand-file-name ".scratch-org/001-alpha" root)))
      (project-scratch-test--write
       root ".scratch-org/001-alpha/issues/01-ready.org"
       "* READY-FOR-AGENT Public agenda ticket\n")
      (my/project-scratch-agenda)
      (should (derived-mode-p 'org-agenda-mode))
      (should (string-match-p
               "Ready.*Public agenda ticket"
               (buffer-substring-no-properties (point-min) (point-max))))
      (goto-char (point-min))
      (search-forward "001  Alpha")
      (button-activate (button-at (line-beginning-position)))
      (should (derived-mode-p 'dired-mode))
      (should (file-equal-p default-directory work-item)))))

(ert-deftest my-project-scratch-find-offers-recursive-org-files ()
  (project-scratch-test--with-project (root)
    (let ((selected
           (project-scratch-test--write
            root ".scratch-org/001-alpha/research/evidence.org"
            "Research evidence\n"))
          offered)
      (project-scratch-test--write
       root ".scratch-org/001-alpha/issues/01-ready.org"
       "* READY-FOR-AGENT Finder ticket\n")
      (project-scratch-test--write
       root ".scratch-org/001-alpha/research/ignored.txt"
       "Not Org\n")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _arguments)
                   (setq offered (all-completions "" collection))
                   "001-alpha/research/evidence.org")))
        (my/project-scratch-find))
      (should (equal
               '("001-alpha/issues/01-ready.org"
                 "001-alpha/research/evidence.org")
               (sort offered #'string<)))
      (should (file-equal-p (buffer-file-name) selected)))))

(ert-deftest my-project-scratch-find-supports-work-item-scopes ()
  (project-scratch-test--with-project (root)
    (let ((selected
           (project-scratch-test--write
            root ".scratch-org/001-alpha/research/evidence.org"
            "Evidence\n"))
          completions)
      (dolist (fixture
               '((".scratch-org/001-alpha/issues/01-ready.org" . "Alpha\n")
                 (".scratch-org/002-beta/issues/01-ready.org" . "Beta\n")
                 (".scratch-org/misc/999 notes.org" . "Unknown scope\n")
                 (".scratch-org/misc/x01 notes.org" . "Noncanonical scope\n")))
        (project-scratch-test--write root (car fixture) (cdr fixture)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _arguments)
                   (setq completions
                         (mapcar
                          (lambda (query)
                            (sort
                             (project-scratch-test--completions
                              query collection)
                             #'string<))
                            '("001 "
                              "001 evidence"
                              "002 "
                              "999 notes"
                              "x01 notes")))
                   "001-alpha/research/evidence.org")))
        (my/project-scratch-find))
      (should
       (equal
        '(("001-alpha/issues/01-ready.org"
           "001-alpha/research/evidence.org")
          ("001-alpha/research/evidence.org")
          ("002-beta/issues/01-ready.org")
          ("misc/999 notes.org")
          ("misc/x01 notes.org"))
        completions))
      (should (file-equal-p (buffer-file-name) selected)))))

(ert-deftest my-project-scratch-agenda-selects-only-tracker-sources ()
  (project-scratch-test--with-project (root)
    (dolist (fixture
             '((".scratch-org/001-alpha/spec.org" . "* READY-FOR-HUMAN Included spec\n")
               (".scratch-org/001-alpha/map.org" . "* IN-PROGRESS Included map\n")
               (".scratch-org/001-alpha/issues/01-ready.org" . "* READY-FOR-AGENT Included issue\n")
               (".scratch-org/001-alpha/issues/nested/02.org" . "* NEEDS-TRIAGE Excluded nested issue\n")
               (".scratch-org/001-alpha/research/evidence.org" . "* NEEDS-TRIAGE Excluded evidence\n")
               (".scratch-org/001-alpha/deployment.org" . "* NEEDS-TRIAGE Excluded deployment\n")
               (".scratch-org/unnumbered/issues/01.org" . "* NEEDS-TRIAGE Excluded unnumbered\n")))
      (project-scratch-test--write root (car fixture) (cdr fixture)))
    (my/project-scratch-agenda)
    (let ((agenda (buffer-substring-no-properties (point-min) (point-max))))
      (should
       (equal
        '(t t t nil nil nil nil)
        (mapcar
         (lambda (title) (and (string-match-p title agenda) t))
         '("Included spec"
           "Included map"
           "Included issue"
           "Excluded nested issue"
           "Excluded evidence"
           "Excluded deployment"
           "Excluded unnumbered")))))))

(ert-deftest my-project-scratch-agenda-refreshes-selected-buffers-from-disk ()
  (project-scratch-test--with-project (root)
    (let* ((selected-file
            (project-scratch-test--write
             root ".scratch-org/001-alpha/issues/01-ready.org"
             "* READY-FOR-AGENT Disk selected\n"))
           (unrelated-file
            (project-scratch-test--write
             root ".scratch-org/001-alpha/research/evidence.org"
             "* Unsaved evidence base\n"))
           (selected-buffer (find-file-noselect selected-file))
           (unrelated-buffer (find-file-noselect unrelated-file)))
      (with-current-buffer selected-buffer
        (erase-buffer)
        (insert "* READY-FOR-AGENT Unsaved selected\n"))
      (with-current-buffer unrelated-buffer
        (erase-buffer)
        (insert "* Unsaved unrelated\n"))
      (my/project-scratch-agenda)
      (should
       (equal
        '("* READY-FOR-AGENT Disk selected\n" nil
          "* Unsaved unrelated\n" t
          t nil)
        (list
         (with-current-buffer selected-buffer (buffer-string))
         (buffer-modified-p selected-buffer)
         (with-current-buffer unrelated-buffer (buffer-string))
         (buffer-modified-p unrelated-buffer)
         (and
          (string-match-p
           "Disk selected"
           (buffer-substring-no-properties (point-min) (point-max)))
          t)
         (and
          (string-match-p
           "Unsaved selected"
           (buffer-substring-no-properties (point-min) (point-max)))
          t))))
      (with-current-buffer selected-buffer
        (erase-buffer)
        (insert "* READY-FOR-AGENT Second unsaved selected\n"))
      (project-scratch-test--write
       root ".scratch-org/001-alpha/issues/01-ready.org"
       "* READY-FOR-AGENT Refreshed disk selected\n")
      (my/project-scratch-agenda-refresh)
      (should
       (equal
        '("* READY-FOR-AGENT Refreshed disk selected\n" nil t nil)
        (list
         (with-current-buffer selected-buffer (buffer-string))
         (buffer-modified-p selected-buffer)
         (and
          (string-match-p
           "Refreshed disk selected"
           (buffer-substring-no-properties (point-min) (point-max)))
          t)
         (and
          (string-match-p
           "Second unsaved selected"
           (buffer-substring-no-properties (point-min) (point-max)))
          t)))))))

(ert-deftest my-project-scratch-agenda-scans-each-source-once ()
  (project-scratch-test--with-project (root)
    (dolist (fixture
             '((".scratch-org/001-alpha/spec.org" . "* READY-FOR-HUMAN Spec\n")
               (".scratch-org/001-alpha/map.org" . "* IN-PROGRESS Map\n")
               (".scratch-org/001-alpha/issues/01.org" . "* READY-FOR-AGENT Issue\n")))
      (project-scratch-test--write root (car fixture) (cdr fixture)))
    (let ((original (symbol-function 'org-map-entries))
          counts)
      (cl-letf (((symbol-function 'org-map-entries)
                 (lambda (&rest arguments)
                   (when (and buffer-file-name
                              (file-in-directory-p buffer-file-name root))
                     (let ((path
                            (file-relative-name buffer-file-name root)))
                       (setf (alist-get path counts nil nil #'equal)
                             (1+ (or (alist-get path counts nil nil #'equal)
                                     0)))))
                   (apply original arguments))))
        (my/project-scratch-agenda)
        (let ((opening-counts (copy-tree counts)))
          (my/project-scratch-agenda-refresh)
          (should
           (equal
            '(((".scratch-org/001-alpha/issues/01.org" . 1)
               (".scratch-org/001-alpha/map.org" . 1)
               (".scratch-org/001-alpha/spec.org" . 1))
              ((".scratch-org/001-alpha/issues/01.org" . 2)
               (".scratch-org/001-alpha/map.org" . 2)
               (".scratch-org/001-alpha/spec.org" . 2)))
            (list
             (sort opening-counts
                   (lambda (left right) (string< (car left) (car right))))
             (sort counts
                   (lambda (left right) (string< (car left) (car right))))))))))))

(ert-deftest my-project-scratch-agenda-renders-ordered-deferred-dates ()
  (project-scratch-test--with-project (root)
    (let ((today (format-time-string "<%Y-%m-%d %a>")))
      (dolist (fixture
               `(("01-triage.org" . "* NEEDS-TRIAGE Triage row\n")
                 ("02-ready.org" . "* READY-FOR-AGENT Ready row\n")
                 ("03-active.org" . "* CLAIMED Active row\n")
                 ("04-overdue.org" . "* DEFERRED Overdue deferred\nSCHEDULED: <2000-01-01 Sat> DEADLINE: <2000-01-02 Sun>\n")
                 ("05-due.org" . ,(format "* DEFERRED Due deferred\nSCHEDULED: %s\n" today))
                 ("06-future.org" . "* DEFERRED Future deferred\nSCHEDULED: <2099-01-01 Thu>\n")
                 ("07-closed.org" . "* RESOLVED Closed row\n")))
        (project-scratch-test--write
         root
         (concat ".scratch-org/001-alpha/issues/" (car fixture))
         (cdr fixture)))
      (my/project-scratch-agenda)
      (let* ((agenda
              (buffer-substring-no-properties (point-min) (point-max)))
             (sections
              (mapcar
               (lambda (header)
                 (string-match (concat "^" header "$") agenda))
               '("Triage" "Ready" "Active" "Deferred" "Closed")))
             (deferred
              (mapcar
               (lambda (title) (string-match title agenda))
               '("Overdue deferred" "Due deferred" "Future deferred"))))
        (should
         (equal
          '(t t
            "[OVERDUE] SCHEDULED: <2000-01-01 Sat> DEADLINE: <2000-01-02 Sun>"
            "[DUE]"
            "SCHEDULED: <2099-01-01 Thu>")
          (list
           (and (cl-every #'integerp sections)
                (apply #'< sections))
           (and (cl-every #'integerp deferred)
                (apply #'< deferred))
           (and
            (string-match-p
             (regexp-quote
              "[OVERDUE] SCHEDULED: <2000-01-01 Sat> DEADLINE: <2000-01-02 Sun>")
             agenda)
            "[OVERDUE] SCHEDULED: <2000-01-01 Sat> DEADLINE: <2000-01-02 Sun>")
           (and (string-match-p (regexp-quote "[DUE]") agenda) "[DUE]")
           (and
            (string-match-p
             (regexp-quote "SCHEDULED: <2099-01-01 Thu>") agenda)
            "SCHEDULED: <2099-01-01 Thu>"))))))))

(ert-deftest my-project-scratch-agenda-retains-org-faces ()
  (project-scratch-test--with-project (root)
    (dolist (fixture
             '(("01-ready.org" . "* READY-FOR-AGENT Ready face\n")
               ("02-resolved.org" . "* RESOLVED Resolved face\n")
               ("03-wontfix.org" . "* WONTFIX Wontfix face\n")))
      (project-scratch-test--write
       root
       (concat ".scratch-org/001-alpha/issues/" (car fixture))
       (cdr fixture)))
    (my/project-scratch-agenda)
    (cl-labels ((face-at
                 (text)
                 (save-excursion
                   (goto-char (point-min))
                   (re-search-forward text)
                   (get-text-property (match-beginning 0) 'face))))
      (should
       (equal
        '(org-agenda-structure org-todo org-done org-done)
        (mapcar #'face-at
                '("^Ready$"
                  "READY-FOR-AGENT"
                  "RESOLVED"
                  "WONTFIX")))))))

(ert-deftest my-project-scratch-agenda-opens-source-at-point ()
  (project-scratch-test--with-project (root)
    (let ((source
           (project-scratch-test--write
            root ".scratch-org/001-alpha/issues/01-ready.org"
            "* READY-FOR-AGENT Open this source\n")))
      (my/project-scratch-agenda)
      (goto-char (point-min))
      (search-forward "Open this source")
      (my/org-agenda-open-at-point)
      (should
       (equal
        (list source "Open this source")
        (list
         (buffer-file-name)
         (org-get-heading t t t t)))))))

(ert-deftest my-project-scratch-agenda-does-not-page-ten-or-fewer-closed-entries ()
  (project-scratch-test--with-project (root)
    (project-scratch-test--write
     root ".scratch-org/001-alpha/issues/01-ready.org"
     "* READY-FOR-AGENT Ready\n")
    (my/project-scratch-agenda)
    (should
     (equal '(() nil)
            (list
             (project-scratch-test--closed-ids)
             (project-scratch-test--load-more-closed-button))))
    (dotimes (index 10)
      (let ((number (+ index 10)))
        (project-scratch-test--write
         root
         (format ".scratch-org/001-alpha/issues/%02d-closed.org" number)
         (format "* RESOLVED Closed %02d\n" number))))
    (my/project-scratch-agenda-refresh)
    (should
     (equal
      (list
       (mapcar (lambda (number) (format "001-%02d" number))
               (number-sequence 19 10 -1))
       nil)
      (list
       (project-scratch-test--closed-ids)
       (project-scratch-test--load-more-closed-button))))))

(ert-deftest my-project-scratch-agenda-paging-row-activates-with-keyboard ()
  (project-scratch-test--with-project (root)
    (dotimes (index 21)
      (let ((number (1+ index)))
        (project-scratch-test--write
         root
         (format ".scratch-org/001-alpha/issues/%02d-closed.org" number)
         (format "* RESOLVED Closed %02d\n" number))))
    (my/project-scratch-agenda)
    (dolist (binding '(("<return>" . line-beginning-position)
                       ("RET" . match-beginning)
                       ("<kp-enter>" . line-beginning-position)
                       ("SPC" . match-beginning)))
      (my/project-scratch-agenda-refresh)
      (goto-char (point-min))
      (re-search-forward "older closed tickets remain")
      (goto-char
       (if (eq (cdr binding) 'match-beginning)
           (match-beginning 0)
         (line-beginning-position)))
      (let ((command (key-binding (kbd (car binding)))))
        (should (eq 'push-button command))
        (call-interactively command))
      (should (= 20 (length (project-scratch-test--closed-ids))))
      (should
       (string-match-p
        "older closed tickets remain"
        (buffer-substring-no-properties
         (line-beginning-position)
         (line-end-position)))))))

(ert-deftest my-project-scratch-agenda-pages-closed-entries-without-rescanning ()
  (project-scratch-test--with-project (root)
    (dotimes (index 23)
      (let ((number (1+ index)))
        (project-scratch-test--write
         root
         (format ".scratch-org/001-alpha/issues/%02d-closed.org" number)
         (format "* RESOLVED Closed %02d\n" number))))
    (my/project-scratch-agenda)
    (should
     (equal
      (mapcar (lambda (number) (format "001-%02d" number))
              (number-sequence 23 14 -1))
      (project-scratch-test--closed-ids)))
    (let ((button (project-scratch-test--load-more-closed-button)))
      (should (equal "13 older closed tickets remain — show next 10"
                     (string-trim (button-label button))))
      (project-scratch-test--write
       root ".scratch-org/001-alpha/issues/23-closed.org"
       "* RESOLVED Changed 23\n")
      (button-activate button))
    (should
     (equal
      (mapcar (lambda (number) (format "001-%02d" number))
              (number-sequence 23 4 -1))
      (project-scratch-test--closed-ids)))
    (should (string-match-p
             "Closed 23"
             (buffer-substring-no-properties (point-min) (point-max))))
    (let ((button (project-scratch-test--load-more-closed-button)))
      (should (equal "3 older closed tickets remain — show next 3"
                     (string-trim (button-label button))))
      (button-activate button))
    (should
     (equal
      (mapcar (lambda (number) (format "001-%02d" number))
              (number-sequence 23 1 -1))
      (project-scratch-test--closed-ids)))
    (should-not (project-scratch-test--load-more-closed-button))
    (should
     (string-match-p
      "001-01"
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position))))
    (my/project-scratch-agenda-refresh)
    (should
     (equal
      (list
       (mapcar (lambda (number) (format "001-%02d" number))
               (number-sequence 23 14 -1))
       t
       "13 older closed tickets remain — show next 10")
      (list
       (project-scratch-test--closed-ids)
       (and (string-match-p
             "Changed 23"
             (buffer-substring-no-properties (point-min) (point-max)))
            t)
       (string-trim
        (button-label
         (project-scratch-test--load-more-closed-button))))))))

;;; project-scratch-test.el ends here
