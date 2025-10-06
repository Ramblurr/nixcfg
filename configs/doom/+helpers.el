;;; $DOOMDIR/+helpers.el -*- lexical-binding: t; -*-

;; Define a specific timeout error (safe if re-evaluated)
(unless (get 'await-timeout 'error-conditions)
  (define-error 'await-timeout "await-callback: timed out"))

(defun await-callback (starter &optional timeout)
  "STARTER is (lambda (resolve reject) ...). Block until it calls one.

TIMEOUT is seconds (float or integer). Defaults to 1.0s.
On timeout, signals `await-timeout'. C-g aborts and signals `quit'.

Example:
  (await-callback
   (lambda (resolve _reject)
     (run-at-time 0.2 nil (lambda () (funcall resolve :ok)))))  ;; => :ok"
  (let* ((timeout (or timeout 1.0))
         (deadline (+ (float-time) timeout))
         (done nil) (value nil) (err nil))
    (funcall starter
             (lambda (v) (setq value v done t))
             (lambda (e) (setq err e done t)))
    (with-local-quit
      (while (not done)
        (when (> (float-time) deadline)
          (signal 'await-timeout (list timeout)))
        (accept-process-output nil 0.1)))
    (when quit-flag
      (setq quit-flag nil)
      (signal 'quit nil))
    (if err (signal (car err) (cdr err)) value)))
