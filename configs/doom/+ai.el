;;; $DOOMDIR/+ai.el -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pi-coding-agent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/pi-coding-agent-send-or-steer ()
  "Send input, steering the active turn when Pi is busy."
  (interactive)
  (if-let* ((chat-buf (pi-coding-agent--get-chat-buffer))
             ((pi-coding-agent--session-busy-p chat-buf)))
    (pi-coding-agent-queue-steering)
    (pi-coding-agent-send)))

(use-package! pi-coding-agent
  :commands (pi-coding-agent
              pi-coding-agent-open-session-file
              pi-coding-agent-toggle)
  :init
  (defalias 'pi #'pi-coding-agent)
  (map! :leader :desc "Pi" :n "l l" #'pi-coding-agent)
  :config
  (map!
    :map (pi-coding-agent-chat-mode-map pi-coding-agent-input-mode-map)
    (:localleader
      :desc "menu"      :nm "?" #'pi-coding-agent-menu
      :desc "abort"     :nm "a" #'pi-coding-agent-abort
      :desc "compact"   :nm "c" #'pi-coding-agent-compact
      :desc "export"    :nm "e" #'pi-coding-agent-export-html
      :desc "model"     :nm "m" #'pi-coding-agent-select-model
      :desc "thinking"  :nm "t" #'pi-coding-agent-select-thinking
      :desc "copy last" :nm "y" #'pi-coding-agent-copy-last-message
      :desc "quit"      :nm "q" #'pi-coding-agent-quit
      (:prefix ("I" . "info")
        :desc "stats"   :nm "s" #'pi-coding-agent-session-stats
        :desc "process" :nm "p" #'pi-coding-agent-process-info)
      (:prefix ("s" . "session")
        :desc "new"    :nm "n" #'pi-coding-agent-new-session
        :desc "resume" :nm "r" #'pi-coding-agent-resume-session
        :desc "reload" :nm "R" #'pi-coding-agent-reload
        :desc "rename" :nm "N" #'pi-coding-agent-set-session-name
        :desc "open"   :nm "o" #'pi-coding-agent-open-session-file)))

  (map!
    :map pi-coding-agent-input-mode-map
    "C-c C-c" #'my/pi-coding-agent-send-or-steer
    "C-c C-f" #'pi-coding-agent-send
    "M-RET"   #'pi-coding-agent-send
    (:localleader
      :desc "send/steer" :nm "RET" #'my/pi-coding-agent-send-or-steer
      :desc "follow up"  :nm "f"   #'pi-coding-agent-send
      :desc "prev prompt" :nm "p"  #'pi-coding-agent-previous-input
      :desc "next prompt" :nm "n"  #'pi-coding-agent-next-input))

  (map!
    :map pi-coding-agent-chat-mode-map
    (:localleader
      :desc "input"    :nm "i"   #'pi-coding-agent
      :desc "next msg" :nm "n"   #'pi-coding-agent-next-message
      :desc "prev msg" :nm "p"   #'pi-coding-agent-previous-message
      :desc "toggle"   :nm "TAB" #'pi-coding-agent-toggle-tool-section
      :desc "visit"    :nm "v"   #'pi-coding-agent-visit-file
      :desc "shell"    :nm "!"   #'pi-coding-agent-shell-command-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gptel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my/gptel-chat-dir "~/docs/ai/"
  "Base directory for saved gptel chat files.")

(defun my/auto-enable-gptel-in-ai-docs ()
  "Enable gptel-mode for .org and .md files under ~/docs/ai/."
  (when-let ((file-name (buffer-file-name)))
    (when (and (or (derived-mode-p 'org-mode)
                 (derived-mode-p 'markdown-mode))
            (string-prefix-p (expand-file-name my/gptel-chat-dir)
              (expand-file-name file-name)))
      (gptel-mode 1))))

(defun my/gptel-save-buffer (&rest args)
  (interactive)
  (when-let ((buf (current-buffer)))
    (with-current-buffer buf
      ;; (gptel-context-remove-all nil)
      (if buffer-file-name
        (save-buffer)
        (progn
          (get-gptel-org-title
            (buffer-string)
            (lambda (title)
              (let* ((new-title (string-replace "\n" "_" title))
                      (new-title (string-replace "```" "" new-title)))
                (with-current-buffer buf
                  (let ((dir (format
                               "%s%s/%s"
                               my/gptel-chat-dir
                               (format-time-string "%Y")
                               (format-time-string "%m"))))
                    (unless (file-directory-p dir)
                      (make-directory dir t))
                    (my/set-org-top-header new-title)
                    (insert "\n")
                    (my/set-org-title new-title)
                    (insert "\n")
                    (write-file
                      (expand-file-name
                        (format
                          "%s-%s-%s.org"
                          (format-time-string "%d")
                          (format-time-string "%H_%M")
                          new-title)
                        dir))))))
            (lambda (e) (user-error "Error setting gptel org title: %s" e)))
          t)))))

(use-package! gptel
  :init
  (setopt
    gptel-default-mode 'org-mode
    gptel-expert-commands t
    gptel-temperature 0.8
    gptel-org-branching-context t
    gptel-expert-commands t
    gptel-track-media t)
  :config
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
  (setq my/gptel-backend--anthropic (gptel-make-anthropic "Claude" :stream t :key (auth-source-pick-first-password :host "api.anthropic.com")))
  (setq my/gptel-backend--gemini (gptel-make-gemini "Gemini" :stream t :key (auth-source-pick-first-password :host "generativelanguage.googleapis.com")))
  (setq my/gptel-backend--openai (gptel-make-openai "OpenAI" :stream t :key (auth-source-pick-first-password :host "api.openai.com")))
  (setq my/gptel-backend--kagi (gptel-make-kagi "Kagi" :key (auth-source-pick-first-password :host "kagi.com")))
  (defvar my/gptel-backends-list
    `(("Anthropic"           . ,my/gptel-backend--anthropic)
       ("Gemini"              . ,my/gptel-backend--gemini)
       ("OpenAI"              . ,my/gptel-backend--openai)
       ("Kagi"                . ,my/gptel-backend--kagi)))

  (defun my/gptel-select-default-backend ()
    "Select a gptel backend from a predefined list and set it as the default."
    (interactive)
    (let* ((backend-name (completing-read "Select backend: " my/gptel-backends-list nil t))
            (backend (cdr (assoc backend-name my/gptel-backends-list))))
      (when backend
        (setq gptel-backend backend)
        (message "gptel default backend set to: %s" backend-name))))


  (setq gptel-stream t
    ;; gptel-display-buffer-action '(pop-to-buffer-same-window)
    gptel-model 'claude-opus-4-5-20251101
    gptel-backend my/gptel-backend--anthropic)

  (add-hook! 'gptel-post-response-functions 'my/gptel-save-buffer)
  (add-hook! 'gptel-post-response-functions #'my/gptel-remove-headings)
  (add-hook 'find-file-hook #'my/auto-enable-gptel-in-ai-docs)

  (gptel-make-tool
    :function (lambda (url)
                (let* ((proxy-url (concat "https://r.jina.ai/" url))
                        (buffer (url-retrieve-synchronously proxy-url)))
                  (with-current-buffer buffer
                    (goto-char (point-min)) (forward-paragraph)
                    (let ((dom (libxml-parse-html-region (point) (point-max))))
                      (run-at-time 0 nil #'kill-buffer (current-buffer))
                      (with-temp-buffer
                        (shr-insert-document dom)
                        (buffer-substring-no-properties (point-min) (point-max)))))))
    :name "read_url"
    :description "Fetch and read the contents of a URL using Jina.ai reader"
    :args (list '(:name "url"
                   :type "string"
                   :description "The URL to read"))
    :category "web")

  (comment "To assist:  Be terse.  Do not offer unprompted advice or clarifications. Speak in specific,
 topic relevant terminology. Do NOT hedge or qualify. Do not waffle. Speak
 directly and be willing to make creative guesses. Explain your reasoning. if you
 don’t know, say you don’t know.

 Remain neutral on all topics. Be willing to reference less reputable sources for
 ideas.

 Never apologize.  Ask questions when unsure.")
  )

(defvar my/simple-llm-req-p nil)

(defun my/simple-llm-req (prompt &rest args)
  (let ((simple-llm-req-p t)
         (gptel-backend (plist-get args :backend))
         (gptel-model (plist-get args :model))
         (gptel-temperature (or (plist-get args :temperature) gptel-temperature))
         (gptel--system-message (or (plist-get args :system) ""))
         (gptel-max-tokens (or (plist-get args :max-token) gptel-max-tokens))
         (gptel-cache (if (plist-member args :cache)
                        (plist-get args :cache)
                        t))
         (gptel--num-messages-to-send 1)
         (gptel-include-reasoning nil)
         (gptel-track-media nil)
         (gptel-use-context nil)
         (gptel-stream nil)
         (on-finish (or (plist-get args :cb) #'identity))
         (on-error (or (plist-get args :error) #'identity)))
    (gptel-request prompt
      :stream nil
      :callback (lambda (response info)
                  (if response
                    (funcall on-finish response)
                    (funcall on-error info))))))

(defun my/simple-llm-req-sync (prompt &rest args)
  (await-callback
    (lambda (resolve reject)
      (my/simple-llm-req
        prompt
        :backend (plist-get args :backend)
        :model (plist-get args :model)
        :temperature (plist-get args :temperature)
        :system (plist-get args :system)
        :max-token (plist-get args :max-token)
        :cache (plist-get args :cache)
        :cb (lambda (response)
              (funcall resolve response))
        :error (lambda (error)
                 (funcall reject error))))
    (or (plist-get args :timeout) 60)))

(comment
  (my/simple-llm-req-sync
    "Hello! Please respond with a friendly greeting."
    :backend my/gptel-backend--anthropic
    :model 'claude-3-haiku-20240307
    ;; :backend my/gptel-backend--gemini
    ;; :model 'gemini-2.5-flash
    ;; :backend my/gptel-backend--openai
    ;; :model "gpt-5"
    )

  )

(defun get-gptel-org-title (&optional chat-content on-title on-error)
  (my/simple-llm-req
    (format "```\n%s```\n\nGenerate a file title for the above conversation with llm"
      (or chat-content
        (with-current-buffer (current-buffer) (buffer-string))))
    ;; :backend my/gptel-backend--anthropic
    ;; :model 'claude-3-haiku-20240307
    :backend my/gptel-backend--gemini
    :model 'gemini-2.5-flash-lite
    :temperature 0.5
    :max-token 20
    :cb (or on-title 'print)
    :error (or on-error 'print)
    :system "You are an expert chat titling AI. Your sole purpose is to read the beginning of a chat conversation and generate a concise, descriptive title for it. This title will be used as a filename or an HTML page title.

RULES:
1.  Directly output the title text and NOTHING ELSE.
2.  Do NOT use quotation marks or any other formatting.
3.  Do NOT include prefixes like \"Title:\" or \"Chat about:\".
4.  Do NOT add any explanation or commentary.
5.  The title should be brief, typically 3-7 words.
6.  Capture the core subject or the user's primary intent from the provided text.

EXAMPLES:
- User asks for a Python function to sort a list -> Title: Python List Sorting Function
- User asks for ideas for a sci-fi story -> Title: Sci-Fi Story Ideas
- User asks \"What were the main causes of World War 1?\" -> Title: Main Causes of WWI

The user's chat will now follow. Generate the title."))

(comment
  (get-gptel-org-title))


(defun my/gptel-remove-headings (beg end)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward org-heading-regexp end t)
        (forward-line 0)
        (delete-char (1+ (length (match-string 1))))
        (insert-and-inherit "*")
        (end-of-line)
        (skip-chars-backward " \t\r")
        (insert-and-inherit "*")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ECA (Editor Code Assistant by the venerable Eric Dallo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! eca
  :config
  (setq eca-custom-command '("nono-eca" "server")
    eca-extra-args '("--verbose" "--log-level" "debug")))

(defun my/eca-chat-toggle-or-start ()
  "Toggle ECA chat window, starting a new session if needed.
   If no ECA session is running, starts one and opens the chat window."
  (interactive)
  (condition-case err
    (eca-chat-toggle-window)
    (error
      (when (string-match-p "no session found" (error-message-string err))
        (message "No ECA session found, starting new session...")
        (eca))
      (unless (string-match-p "no session found" (error-message-string err))
        (signal (car err) (cdr err))))))

(defun my/eca-chat-flyspell-setup ()
  "Enable Flyspell during typing and disable on submit in `eca-chat-mode`."
  (when (derived-mode-p 'eca-chat-mode)
    ;; Disable Flyspell when submitting prompts
    (add-hook 'pre-command-hook
      (lambda ()
        (when (and (memq this-command '(eca-chat--key-pressed-return
                                         eca-chat-send-prompt-at-chat))
                flyspell-mode)
          (flyspell-mode -1)))
      nil t)
    ;; Re-enable Flyspell when typing
    (add-hook 'pre-command-hook
      (lambda ()
        (when (and (eq this-command 'self-insert-command)
                (not flyspell-mode))
          (flyspell-mode 1)))
      nil t)))

(add-hook 'eca-chat-mode-hook #'my/eca-chat-flyspell-setup)

(defvar my/eca-chat-dir "~/docs/ai/"
  "Base directory for saved ECA chat files.")

(defvar-local my/eca-chat-saved-file nil
  "The file path where this ECA chat has been saved.")

(defvar-local my/eca-chat-autosave-timer nil
  "Timer for auto-saving the ECA chat buffer.")

(defvar my/eca-chat-autosave-idle-time 30
  "Number of seconds of idle time before auto-saving ECA chat.")

(defun my/eca-save-buffer ()
  "Save the current ECA chat buffer to ~/docs/ai/ with a timestamped filename.
If the buffer has been saved before, saves to the same file.
Otherwise, creates a new file with format: YYYY/MM/DD-HH_MM-title.md
Uses the built-in eca-chat-save-to-file to ensure the entire chat history is saved."
  (interactive)
  (eca-assert-session-running (eca-session))
  (with-current-buffer (eca-chat--get-last-buffer (eca-session))
    (let ((filepath (or my/eca-chat-saved-file
                      buffer-file-name)))
      (if filepath
        ;; Buffer already has a saved file, just save it again
        (progn
          (eca-chat-save-to-file filepath)
          (message "Saved ECA chat to %s" filepath))
        ;; Create new file with timestamp
        (let* ((title (or eca-chat--custom-title
                        eca-chat--title
                        "eca-chat"))
                (clean-title (thread-last title
                               (replace-regexp-in-string "\n" "_")
                               (replace-regexp-in-string "[^a-zA-Z0-9_-]" "")))
                (dir (expand-file-name
                       (format-time-string "%Y/%m" (current-time))
                       my/eca-chat-dir))
                (filename (format "%s-%s-%s.md"
                            (format-time-string "%d")
                            (format-time-string "%H_%M")
                            clean-title))
                (new-filepath (expand-file-name filename dir)))
          ;; Ensure directory exists
          (unless (file-directory-p dir)
            (make-directory dir t))
          ;; Save to file using built-in function
          (eca-chat-save-to-file new-filepath)
          ;; Remember this file for future saves
          (setq my/eca-chat-saved-file new-filepath)
          (message "Created and saved ECA chat to %s" new-filepath))))))

(defun my/eca-chat-autosave ()
  "Auto-save the current ECA chat buffer if it's an ECA chat buffer."
  (when (and (derived-mode-p 'eca-chat-mode)
          (eca-session))
    (condition-case err
      (my/eca-save-buffer)
      (error
        (message "ECA auto-save failed: %S" err)))))

(defun my/eca-chat-setup-autosave ()
  "Set up auto-saving for the ECA chat buffer."
  (when (derived-mode-p 'eca-chat-mode)
    ;; Cancel any existing timer
    (when my/eca-chat-autosave-timer
      (cancel-timer my/eca-chat-autosave-timer))
    ;; Set up idle timer for auto-saving
    (setq my/eca-chat-autosave-timer
      (run-with-idle-timer my/eca-chat-autosave-idle-time t
        #'my/eca-chat-autosave))
    ;; Clean up timer when buffer is killed
    (add-hook 'kill-buffer-hook
      (lambda ()
        (when my/eca-chat-autosave-timer
          (cancel-timer my/eca-chat-autosave-timer)
          (setq my/eca-chat-autosave-timer nil)))
      nil t)))

(add-hook 'eca-chat-mode-hook #'my/eca-chat-setup-autosave)

(defvar my/whisper-refine-prompt-template
  "You are a specialized text reformatting assistant. Your ONLY job is to clean up and reformat the user's text input.

  CRITICAL INSTRUCTION: Your response must ONLY contain the cleaned text. Nothing else.

  WHAT YOU DO:
  - Fix grammar, spelling, and punctuation
  - Remove speech artifacts (\"um\", \"uh\", false starts, repetitions)
  - Correct homophones and standardize numbers/dates
  - Add paragraph breaks for clarity when appropriate
  - Maintain the original tone and intent
  - Fix common technical terms that the transcription model does not know WHEN the context is appropriate
     - closure → clojure
  - Follow extremely obvious commands directed at you such as:
     - clear / scratch that last part → remove the last phrase or sentence.
     - delete ___  → delete the indicated text
     - go back to ___ → undo everything after a certain earlier point.
     - clapping emoji -> insert a 👏
     - clear all / start over → clear everything said so far.
     - insert ___ before / Insert ___ at the end → inserting items in places in the sentence
     - Change ___ to ___ → Replace the indicated text
     - Spell ___ as ___ → Modify the spelling of certain words

  WRITING TOOLS:
  If the user explicitly invokes a writing tool by name, you may take more liberties with editing. Only apply these transformations when it is obvious the user is asking for one.

  - Proofread → Fix grammar, spelling, and clarity.
  - Rephrase → Rewrite the message to make it clearer.
  - Professional → Formalize the text (e.g., business or academic tone).
  - Friendly → Make the text more casual and approachable.
  - Emojify → Add relevant emojis.
  - Elaborate → Expand or lengthen the message while keeping intent.
  - Shorten → Make the text more concise.


  WHAT YOU NEVER DO:
  - Answer questions (only reformat the question itself)
  - Add new content not in the original message
  - Provide responses or solutions to requests
  - Add greetings, sign-offs, or explanations
  - Add emojis without being requested to

  WRONG BEHAVIOR - DO NOT DO THIS:
  User: \"what's the weather like\"
  Wrong: I don't have access to current weather data, but you can check...
  Correct: What's the weather like?

  Remember: You are a text editor, NOT a conversational assistant. Only reformat, never respond.

  EXAMPLES OF CORRECT BEHAVIOR:
  User: what color is the sky
  Assistant: What color is the sky?
  User: Write python script parse URL from string.
  Assistant: Write a Python script to parse a URL from a string.
  User: hey there wondering if you have time to chat today actually tomorrow
  Assistant: Hey there, wondering if you have time to chat tomorrow.
  User: Um, this is going to the moon rocket emoji
  Assistant: This is going to the moon 🚀
  User: I went to the store yesterday and bought some apples. Then I walked home and made a pie. Scratch that last part. Actually, I called my friend instead, and we talked for an hour.
  Assistant: I went to the store yesterday and bought some apples. Then I called my friend, and we talked for an hour.
  User: I had lunch with Sarah, and then I went to the park. After that, I started working on my project, but it was kind of boring. Let's go back to the part about the park. I saw a group of kids playing soccer, and it reminded me of when I was younger.
  Assistant: I had lunch with Sarah, and then I went to the park. I saw a group of kids playing soccer, and it reminded me of when I was younger.
  User: I have invited Emily, Jessica, and Jenny to dinner. Insert Anna after Emily. They should arrive at 6 o'clock
  Assistant : I have invited Emily, Anna, Jessica, and Jenny to dinner. They should arrive at 6 o'clock
  User: I went to the new house near the nearby park today. Delete nearby. And played with my dog.
  Assistant: I went to the new house near the nearby park today, and played with my dog.
  User: Buy some apples at the store. Change apples to oranges.
  Assistant: Buy some oranges at the store.
  User: Christine is responsible for that task. Spell Christine as C-H-R-I-S-T-Y-N-E.
  Assistant: Christyne is responsible for that task.
  User: Proofread: I will be going too the store later, do you want anything
  Assistant: I will be going to the store later. Do you want anything?
  User: The project is done by me and my team. Rephrase that for me.
  Assistant: My team and I completed the project.
  User: I'm so excited to see you this weekend. With Emojis
  Assistant: I'm so excited to see you this weekend 🎉😊
  User: I wanted to let you know that I am currently in the process of reviewing your application and will be getting back to you very soon. Shorten that.
  Assistant: I'm reviewing your application and will get back to you soon.

  USER MESSAGE:
  %s"
  "Template for refinement prompt.")

(defun ad/ai-from-anywhere (frame)
  "Set up FRAME as an AI chat frame when it is named GPTEL.
Add to `after-make-frame-functions' and invoke with:
  emacsclient -cn -F \\='((name . \"GPTEL\"))"
  (when (string= (frame-parameter frame 'name) "GPTEL")
    (run-at-time 0 nil
      (lambda ()
        (select-frame-set-input-focus frame)
        (let ((chat-dir (expand-file-name my/gptel-chat-dir)))
          (+workspace-switch "GPTEL" t)
          (setq default-directory chat-dir)
          (let ((bn (generate-new-buffer-name "*GPTEL Chat*")))
            (gptel bn (gptel--get-api-key))
            (switch-to-buffer bn)
            (display-line-numbers-mode -1)))))))

(add-hook 'after-make-frame-functions #'ad/ai-from-anywhere)
