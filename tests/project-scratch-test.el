;;; project-scratch-test.el --- Project scratch command tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'org-agenda)
(require 'project)

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
                   (setq offered collection)
                   "001-alpha/research/evidence.org")))
        (my/project-scratch-find))
      (should (equal
               '("001-alpha/issues/01-ready.org"
                 "001-alpha/research/evidence.org")
               (sort offered #'string<)))
      (should (file-equal-p (buffer-file-name) selected)))))

;;; project-scratch-test.el ends here
