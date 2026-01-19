;;; +my-commands.el -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(comment (evil-define-command my/lispyville-insert-at-end-of-list (count)
           "Same as `lispyville-insert-at-end-of-list', but adds a newline."
           (interactive "<c>")
           ;; TODO if already at the end of the list, add a newline above
           (when (lispyville--out-forward (or count 1))
             (backward-char)
             (newline-and-indent)
             (evil-change-state lispyville-preferred-state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Clojure commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; evaluate some code in a CLJ repl regardless of what type of buffer you're in
(defun my/eval-clojure-code-sync (code)
  (cider-nrepl-sync-request:eval
   code
   (cider-current-repl 'clj 'ensure)
   (cider-current-ns)))

(defun my/destructure-clojure-param ()
  "Convert a simple parameter to a destructured Clojure map parameter.
When called with point over a parameter, it transforms it to
'{:keys [] :as param}' and puts the cursor between the square brackets."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (param (when bounds (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (has-space-after (when bounds
                            (and (< (cdr bounds) (point-max))
                                 (eq (char-after (cdr bounds)) ?\s)))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (format "{:keys [] :as %s}" param))
      (when has-space-after
        (insert " "))
      (backward-char (+ (if has-space-after 8 7) (length param))))))

;; Dev reset - reload
(defun my/clojure-dev-reset ()
  (interactive)
  (cider-interactive-eval
   ;; (format "(ol.app.dev.dev-extras/reset)"
   ;;         (cider-last-sexp))

   (format "(dev/reset)"
           (cider-last-sexp))))

;; Rich Comment Form evaluator
;; Evaluates the *last* form in a clojure buffer that has the ';; rcf' before it.
(defun my/eval-rcf ()
  (interactive) ;; you have to do this to be able to bind it to a key or M-x it
  (save-excursion      ;; this is what returns the point to where you started
    (goto-char (point-max))              ;; go to end of buffer
    (if (re-search-backward " *;; rcf" nil t) ;; search backward, don't throw an error
        (cider-eval-last-sexp) ;; the cider fn which evals the last form
      (message "No rcf found!")))) ;; if the search fails, show a message in the echo area

;; Refresh browser command (for non-cljs projects like when using htmx)
(defun my/clojure-reload-browser ()
  "Reload the browser on eval"
  (interactive)
  (cider-interactive-eval
   (format "(browser/refresh)" (cider-last-sexp))))


;; Like my/clojure-reload-browser expect evals the defun first
(defun my/eval-defun-and-reload-browser ()
  "Reload the browser on eval"
  (interactive)
  (cider-eval-defun-at-point)
  (my/clojure-reload-browser))

(defun my/clojure-clean-and-sort-ns ()
  "Clean and sort the clojure active namespace.
After cleaning and sorting, collapses all import groups back to compact form,
then wraps long lines at 90 columns with 4-space indentation."
  (interactive)
  (lsp-clojure-clean-ns)
  (clojure-sort-ns)
  ;; I don't like how sort-ns puts :import classes on their own line
  ;; So this finds and collapses those import statements but with 90 col wrapping
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[[:space:]]*(:import" nil t)
      (goto-char (match-beginning 0))
      (let ((import-start (point)))
        ;; Move to the end of the import form to establish bounds
        (forward-char)  ; Move past the opening paren
        (forward-sexp)  ; Move to closing paren
        (backward-char) ; Move back inside
        (let ((import-end (point)))
          ;; Now process each opening bracket within the import form
          (goto-char import-start)
          ;; Skip past "(:import" to start searching for brackets
          (forward-char 8)
          (while (and (< (point) import-end)
                      (re-search-forward "\\[" import-end t))
            (backward-char) ; Move back onto the bracket
            (let ((bracket-start (point)))
              (when (fboundp 'symex-collapse)
                (symex-collapse))
              ;; Wrap long lines at 90 columns
              (save-excursion
                (goto-char bracket-start)
                (let* ((line-start (line-beginning-position))
                       (base-indent (- bracket-start line-start)))
                  (forward-char) ; Skip opening bracket
                  (forward-sexp 1) ; Skip package name
                  (while (not (looking-at "\\s-*\\]"))
                    (skip-chars-forward " \t")
                    (let ((before-class (point)))
                      ;; Skip to end of current class name
                      (skip-chars-forward "^] \t\n")
                      ;; Check if line is too long
                      (when (> (current-column) 90)
                        ;; Go back to before this class name and insert newline
                        (goto-char before-class)
                        (insert "\n")
                        (insert (make-string (+ base-indent 1) ?\s)))))))
              ;; Skip past the entire form
              (goto-char bracket-start)
              (forward-sexp)
              ;; Update import-end since modifications changed buffer positions
              (save-excursion
                (goto-char import-start)
                (forward-char)
                (forward-sexp)
                (setq import-end (1- (point)))))))))))

(defun my/markdown-table-align-in-docstring ()
  "Align the markdown table in the docstring at point.

Assumes point is inside a string (typically inside the table).
Temporarily inserts a newline *before the docstring’s closing quote* so the
quote can’t be treated as part of the last table row, runs
`markdown-table-align`, then removes that newline again."
  (interactive)
  (let* ((ppss (syntax-ppss))
         (string-start (nth 8 ppss)))
    (unless string-start
      (user-error "Point is not inside a string"))
    (let* ((orig (point))
           (string-end (scan-sexps string-start 1)) ; position AFTER closing quote
           (close-quote-pos (1- string-end))
           ;; IMPORTANT: insertion-type = t so the marker stays attached to the quote
           ;; even when we insert text at its position.
           (quote-marker (copy-marker close-quote-pos t))
           (close-was-inline
            (save-excursion
              (goto-char (marker-position quote-marker))
              (not (eq (char-before) ?\n)))))
      (unwind-protect
          (save-excursion
            ;; Put closing quote on its own line if it wasn't already.
            (when close-was-inline
              (goto-char (marker-position quote-marker))
              (unless (eq (char-after) ?\")
                (user-error "Expected closing double-quote at end of string"))
              (insert "\n"))

            ;; Ensure point is on a table line before aligning.
            (goto-char orig)
            (unless (looking-at-p "^[ \t]*|")
              (or (re-search-backward "^[ \t]*|" string-start t)
                  (re-search-forward "^[ \t]*|" (marker-position quote-marker) t)
                  (user-error "No markdown table line ('|') found in this docstring")))
            (beginning-of-line)
            (markdown-table-align))
        ;; Restore: remove the newline we inserted immediately before the closing quote.
        (when close-was-inline
          (save-excursion
            (goto-char (marker-position quote-marker))
            (when (and (eq (char-after) ?\")
                       (eq (char-before) ?\n))
              (delete-char -1)))))
      (goto-char orig))))

(defun my/markdown-align-all-docstring-tables-in-buffer ()
  "Align all markdown tables inside docstrings in the current buffer.

Implemented in terms of `my/markdown-table-align-in-docstring`.
Preserves point and window start."
  (interactive)
  (let ((orig (point))
        (orig-win-start (window-start)))
    (unwind-protect
        (save-excursion
          (goto-char (point-min))
          ;; Find each string, then within it find each table block and align once per block.
          (while (re-search-forward "\"" nil t)
            (let* ((ppss (syntax-ppss))
                   (string-start (nth 8 ppss)))
              (when (and (nth 3 ppss) string-start
                         (= (point) (1+ string-start))) ; at opening quote
                (let* ((string-end (scan-sexps string-start 1))
                       (string-limit (1- string-end))) ; closing quote position
                  (save-excursion
                    (goto-char (1+ string-start))
                    (while (re-search-forward "^[ \t]*|" string-limit t)
                      ;; Align this table (point must be on a table line)
                      (beginning-of-line)
                      (my/markdown-table-align-in-docstring)
                      ;; Skip past this table block so we don't realign each row.
                      (while (and (< (point) string-limit)
                                  (save-excursion
                                    (beginning-of-line)
                                    (looking-at-p "^[ \t]*|")))
                        (forward-line 1))))))))))
    (goto-char orig)
    (set-window-start (selected-window) orig-win-start)))

(defun my/yank-ns-name ()
  "Yank clojure namespace name"
  (interactive)
  (let ((ns (walkclj-current-ns)))
    (kill-new ns)
    (message "Copied: %s" ns)))


(comment
 ;; Test with this sample namespace
 "(ns ol.clave.scope
  \"Lightweight cancellation/deadline scopes used to propagate structured context
  through ACME plumbing layers.\"
  (:require
   [ol.clave.errors :as errors])
  (:import
   [java.lang.ref WeakReference]
   [java.time Duration Instant]
   [java.util UUID]
   [java.util.concurrent
    Callable
    StructuredTaskScope
    StructuredTaskScope$FailedException
    StructuredTaskScope$Joiner
    StructuredTaskScope$Subtask
    StructuredTaskScope$Subtask$State
    StructuredTaskScope$TimeoutException]
   [java.util.function Predicate]))"
 )

;; Integration with portal

(defun portal.api/clear ()
  (interactive)
  (my/eval-clojure-code-sync "(portal.api/clear)")
  ;; (cider-nrepl-sync-request:eval "#?(:clj (portal.api/clear))")
  ;; (cider-nrepl-sync-request:eval "#?(:cljs (portal.web/clear))")
  ;; (cider-nrepl-sync-request:eval "(#?(:clj portal.api/clear :cljs portal.web/clear))")
  )

(defun portal/invoke-portal-command (command-str)
  (cider-nrepl-sync-request:eval
   (concat "(#?(:clj portal.api/eval-str :cljs portal.web/eval-str) \"" command-str "\")")))

(defun portal.ui.commands/select-root ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-root portal.ui.state/state)"))

(defun portal.ui.commands/select-next ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-next portal.ui.state/state)"))

(defun portal.ui.commands/select-prev ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-prev portal.ui.state/state)"))

(defun portal.ui.commands/select-parent ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-parent portal.ui.state/state)"))

(defun portal.ui.commands/select-child ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-child portal.ui.state/state)"))

(defun portal.ui.commands/history-back ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/history-back portal.ui.state/state)"))

(defun portal.ui.commands/focus-selected ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/focus-selected portal.ui.state/state)"))

(defun portal.ui.commands/set-tree-viewer ()
  (interactive)
  (portal/invoke-portal-command
   "(require '[portal.ui.state :as s])

    (defn set-viewer! [viewer]
      (s/dispatch!
       s/state
       assoc-in
       [:selected-viewers
        (s/get-location
         (s/get-selected-context @s/state))]
       viewer))

    (set-viewer! :portal.viewer/tree)"))


;; make w/e/b move by words inside strings and comments
(comment (defun fn--lispyville-e-handler ()
           (interactive)
           (if (or (in-string-p)
                   (evilnc-pure-comment-p (point))
                   )
               (evil-forward-word-end)
             (lispyville-forward-atom-end)))

         (defun fn--lispyville-w-handler ()
           (interactive)
           (if (or (in-string-p)
                   (evilnc-pure-comment-p (point))
                   )
               (evil-forward-word-begin)
             (lispyville-forward-atom-begin)))

         (defun fn--lispyville-b-handler ()
           (interactive)
           (if (or (in-string-p)
                   (evilnc-pure-comment-p (point)))
               (evil-backward-word-begin)
             (lispyville-backward-atom-begin))))


;; from https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
(defun my/reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my/reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the current buffer's,
reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))

(defun my/cider-set-print-length ()
  (interactive)
  (cider-interactive-eval "(set! *print-length* 100)"))

(defun my/copy-latest-commit-hash ()
  "Copy the latest commit hash of the current project to the clipboard."
  (interactive)
  (let ((commit-hash (magit-git-string "rev-parse" "HEAD")))
    (kill-new commit-hash)
    (message "Copied commit hash: %s" commit-hash)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System/OS commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my/system--speaker-was-muted nil
  "Track whether speaker was muted before unmuting.")

(defvar my/system--mic-was-muted nil
  "Track whether mic was muted before unmuting.")

(defun my/system--mic-mute ()
  "Mute the system mic unconditionally."
  (call-process "mic-mute"))

(defun my/system--mic-unmute ()
  "Unmute the system mic and remember the previous mute state."
  (setq my/system--mic-was-muted
        (string= "1" (string-trim (shell-command-to-string "mic-get-mute"))))
  (call-process "mic-unmute"))

(defun my/system--mic-mute-revert ()
  "Revert mic to previous mute state.
  If mic was muted before unmute, mute them again."
  (when my/system--mic-was-muted
    (call-process "mic-mute")))

(defun my/disable-all-active-themes ()
  "Disable all currently active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun my/lsp-workspace-remove-missing-projects ()
  (interactive)
  (dolist (dead-project (seq-filter (lambda (x) (not (file-directory-p x))) (lsp-session-folders (lsp-session))))
    (lsp-workspace-folders-remove dead-project)))
