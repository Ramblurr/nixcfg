;;; $DOOMDIR/+ai.el -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gptel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! gptel
  :config
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
        gptel-model 'claude-sonnet-4-20250514
        gptel-backend my/gptel-backend--anthropic)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copilot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Migrated to using lsp's builtin copilot suport

(comment
 (use-package! copilot
   :hook (prog-mode . copilot-mode)
   :bind (:map copilot-completion-map
               ("<right>" . 'copilot-accept-completion)
               ("C-<right>" . 'copilot-accept-completion-by-word))
   :custom (copilot-max-char-warning-disable t)
   :config
   (setq copilot-indent-offset-warning-disable t)
   (setq copilot-log-max 10000)
   (customize-set-variable 'copilot-enable-predicates '(evil-insert-state-p))
   (custom-theme-set-faces! '(doom-gruvbox)
     `(copilot-overlay-face  :foreground ,(doom-color 'violet) :underline t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ECA (Editor Code Assistant by the venerable Eric Dallo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(use-package! eca
  :config
  (setq eca-extra-args '("--verbose" "--log-level" "debug")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whisper.el on NixOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used by ECA, but also my custom usage

;; SETUP:
;; Using whisper.el on NixOS
;; 1. install whisper-cpp from nixpkgs
;; 2. create  ~/.config/emacs/.local/etc/whisper/whisper.cpp/models/
;; 3. cd into that dir and use whisper-cpp-download-ggml-model to download the model you want


(defun my/whisper--nix-command (input-file)
  `((executable-find "whisper-cli")
    "--model" ,(expand-file-name (concat doom-data-dir "whisper/whisper.cpp/models/ggml-" whisper-model ".bin"))
    ,@(when whisper-use-threads (list "--threads" (number-to-string whisper-use-threads)))
    ,@(when whisper-translate '("--translate"))
    ,@(when whisper-show-progress-in-mode-line '("--print-progress"))
    "--language" ,whisper-language
    "--no-timestamps"
    "--file" ,input-file))

(defun my/whisper--find-whispercpp-server ()
  "Find whisper.cpp server binary in PATH."
  (executable-find "whisper-server"))

(defvar my/whisper-session nil
  "Current whisper session plist with mode-specific structure:
- Buffer mode: (:mode buffer :buffer buf :marker pos :refine t/nil)
- Terminal mode: (:mode terminal :buffer buf :refine t/nil)
- EXWM mode: (:mode exwm :buffer buf :refine t/nil)")

(defvar my/whisper-refine-backend
  ;; my/gptel-backend--gemini
  my/gptel-backend--anthropic
  "Backend to use for text refinement.")

(defvar my/whisper-refine-model
  ;; "gemini-2.5-flash-lite"
  'claude-3-haiku-20240307
  "Model to use for text refinement.")

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

(defun my/whisper-cleanup-session ()
  "Clean up whisper session state."
  (when my/whisper-session
    (let ((marker (plist-get my/whisper-session :marker)))
      (when marker (set-marker marker nil)))
    (setq my/whisper-session nil)))

(defun my/whisper-cleanup-history-buffers ()
  "Clean up all timestamped whisper history buffers."
  (interactive)
  (let ((killed-count 0))
    (dolist (buffer (buffer-list))
      (when (string-match-p "\\*whisper-[0-9]+\\*" (buffer-name buffer))
        (kill-buffer buffer)
        (setq killed-count (1+ killed-count))))
    (message "Cleaned up %d whisper history buffers" killed-count)))

(defun my/whisper-refine-text (text)
  "Refine text using configured model. Returns refined text or original if refinement fails."
  (message "Refining...")
  (let* ((prompt (format my/whisper-refine-prompt-template text))
         (refined (condition-case err
                      (string-trim
                       (my/simple-llm-req-sync
                        prompt
                        :backend my/whisper-refine-backend
                        :model my/whisper-refine-model))
                    (error
                     (message "Refinement failed: %S" err)
                     nil))))
    (if (or (not refined) (string-empty-p refined))
        text
      refined)))

(defun my/whisper-process-text ()
  "Process transcribed text with optional refinement."
  (let ((text (string-trim (buffer-string))))
    (when (and text
               (not (string-empty-p text))
               my/whisper-session)
      (let* ((buffer (plist-get my/whisper-session :buffer))
             (buffer-name (plist-get my/whisper-session :buffer-name))
             (mode (plist-get my/whisper-session :mode))
             (final-text (if (plist-get my/whisper-session :refine)
                             (my/whisper-refine-text text)
                           text)))
        (pcase mode
          ('terminal
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (vterm-send-string final-text))))
          ('exwm
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (kill-new final-text)
               (exwm-input--fake-key ?\C-v))))
          ('eca-chat
           (eca-chat--with-current-buffer
               buffer-name
             (goto-char (point-max))
             (insert final-text)
             (newline)
             (eca-chat--key-pressed-return)))
          ('buffer
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when-let ((marker (plist-get my/whisper-session :marker)))
                 (goto-char marker))
               (insert final-text)))))
        (message "Text %s%s"
                 (if (plist-get my/whisper-session :refine) "refined and " "")
                 (pcase mode
                   ('terminal "sent to terminal")
                   ('exwm "typed to EXWM window")
                   ('eca-chat "sent to ECA chat")
                   ('buffer "inserted")))
        (my/whisper-cleanup-session)))))

(defun my/whisper-suppress-display (orig-fun &rest args)
  "Suppress buffer display when using whisper."
  (if my/whisper-session
      (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
        (apply orig-fun args))
    (apply orig-fun args)))

(defun my/whisper--copy-result ()
  "A hook used to copy transcription result to clipboard"
  (let ((start (point-min))
        (end (point-max)))
    (copy-region-as-kill start end)))

(use-package! whisper
  :demand t
  :hook
  (whisper-before-transcription-hook . my/system--mic-unmute)
  (whisper-after-transcription-hook .  my/system--mic-mute-revert)
  (whisper-after-transcription-hook .  my/whisper--copy-result)
  (whisper-after-transcription-hook .  my/whisper-process-text)
  :config
  (setq
   whisper-install-directory (concat doom-data-dir "whisper")
   whisper-install-whispercpp nil ;; use from path (nixpkgs in my case)
   whisper-display-transcription-buffer nil
   whisper-insert-text-at-point nil
   whisper-model "large-v3-turbo"
   whisper-language "en"
   whisper-translate nil
   whisper-use-threads (/ (num-processors) 2)
   whisper-server-mode 'local
   whisper-server-host "127.0.0.1"
   whisper-server-port 8642)

  (defvar my/whisper--saved-insert-at-point nil
    "Saved value of whisper-insert-text-at-point.")

  (defvar my/whisper--saved-display-buffer nil
    "Saved value of whisper-display-transcription-buffer.")

  (defun my/whisper--record-to-buffer-cleanup ()
    "Cleanup function for whisper-record-to-buffer."
    (setq whisper-insert-text-at-point my/whisper--saved-insert-at-point
          whisper-display-transcription-buffer my/whisper--saved-display-buffer)
    (remove-hook 'whisper-after-transcription-hook #'my/whisper--record-to-buffer-cleanup))

  (defun my/whisper-record-to-buffer ()
    "Start background recording with whisper.el and show result in buffer."
    (interactive)
    (unless (whisper-recording-p)
      (setq my/whisper--saved-insert-at-point whisper-insert-text-at-point
            my/whisper--saved-display-buffer whisper-display-transcription-buffer)
      (setq whisper-insert-text-at-point nil
            whisper-display-transcription-buffer t)
      (add-hook 'whisper-after-transcription-hook #'my/whisper--record-to-buffer-cleanup))
    (whisper-run))


  (defmacro my/define-whisper-hydra (name language display-name)
    `(defhydra ,name (:color pink :hint nil :body-pre (unless (whisper-recording-p) (my/whisper ,language)))
       ,(format "
  Whisper %s [Refine: %%(if (and my/whisper-session (plist-get my/whisper-session :refine)) \"ON\" \"OFF\")]

  _r_: Toggle refine  _c_: Clean history  _q_: %%(if (whisper-recording-p) \"Stop & Transcribe\" \"Cancel\")
  " display-name)
       ("r" (when my/whisper-session
              (plist-put my/whisper-session :refine
                         (not (plist-get my/whisper-session :refine)))))
       ("c" (my/whisper-cleanup-history-buffers))
       ("q" (if (whisper-recording-p)
                (interrupt-process whisper--recording-process)
              (message "Cancelled"))
        :exit t)))

  (my/define-whisper-hydra my/whisper-english-hydra "en" "English")

  (defun my/whisper (&optional language)
    "Transcribe audio - smart handling for vterm vs normal buffers. LANGUAGE defaults to auto-detect."
    (interactive)
    (if (whisper-recording-p)
        (interrupt-process whisper--recording-process)
      (setq whisper-language (or language "auto"))
      ;; Only create a new session if one doesn't already exist
      (unless my/whisper-session
        (setq my/whisper-session
              (pcase major-mode
                ('vterm-mode (list :mode 'terminal :buffer (current-buffer) :refine nil))
                ('exwm-mode (list :mode 'exwm :buffer (current-buffer) :refine nil))
                (_ (list :mode 'buffer :buffer (current-buffer) :refine nil :marker (point-marker))))))
      (whisper-run)))

  (defun my/eca-chat-talk (&optional language)
    "Talk to ECA assistant by recording audio and transcribing it.
LANGUAGE defaults to auto-detect. Use the hydra to toggle refinement."
    (interactive)
    (unless (require 'whisper nil t)
      (user-error "Whisper.el is not available, please install it first"))
    (let* ((session (eca-session))
           (chat-buffer-name (eca-chat-buffer-name session)))
      (eca-assert-session-running session)
      (eca-chat-open session)
      (eca-chat--with-current-buffer
          chat-buffer-name
        (goto-char (point-max)))
      (setq whisper-language (or language "auto"))
      (setq my/whisper-session
            (list :mode 'eca-chat
                  :buffer (get-buffer chat-buffer-name)
                  :buffer-name chat-buffer-name
                  :refine nil))
      (my/whisper-english-hydra/body)))


  (advice-add 'whisper-command :override #'my/whisper--nix-command)
  (advice-add 'whisper--find-whispercpp-server :override #'my/whisper--find-whispercpp-server)
  (advice-add 'whisper--handle-transcription-output :around #'my/whisper-suppress-display) )




;; (setq whisper-after-transcription-hook (cdr whisper-after-transcription-hook))
