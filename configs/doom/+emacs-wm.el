;; emacs-wm.el -*- lexical-binding: t -*-
;; From Pavel Korytov, https://sqrtminusone.xyz/posts/2021-10-04-emacs-i3/
;; and https://github.com/karthink/.emacs.d/blob/84fc5f5efac3f7b8b318fe87f62f4ae9246c48ae/plugins/emacs-wm.el
;; modified for hyprland in 2024 by @ramblurr

(require 'windmove)
(require 'transpose-frame)

(defmacro comment (&rest body)
  "Ignore BODY, yields nil."
  nil)

;;----------------------------------------------------------------
;; ** i3 integration
;;----------------------------------------------------------------

(eval-when-compile
  (defmacro i3-msg (&rest args)
    `(start-process "emacs-i3-windmove" nil "i3-msg" ,@args)))

(defun my/emacs-i3-windmove (direction)
  (let ((other-window (windmove-find-other-window direction)))
    (if (or (null other-window) (window-minibuffer-p other-window))
        (i3-msg "focus" (symbol-name direction))
      (windmove-do-window-select direction))))

(defun my/emacs-i3-direction-exists-p (dir)
  (seq-some (lambda (dir)
              (let ((win (windmove-find-other-window dir)))
                (and win (not (window-minibuffer-p win)))))
            (pcase dir
              ('width '(left right))
              ('height '(up down)))))

(defun my/emacs-i3-move-window (dir &rest _)
  (let ((other-window (windmove-find-other-window dir))
        (other-direction (my/emacs-i3-direction-exists-p
                          (pcase dir
                            ('up 'width)
                            ('down 'width)
                            ('left 'height)
                            ('right 'height)))))
    (cond
     ((and other-window (not (window-minibuffer-p other-window)))
      (window-swap-states (selected-window) other-window))
     (other-direction
      (evil-move-window dir))
     (t (i3-msg "move" (symbol-name dir))))))

(defun my/emacs-i3-integration (command)
  (pcase command
    ((rx bos "focus")
     (my/emacs-i3-windmove
      (intern (elt (split-string command) 1))))
    ((rx bos "move")
     (my/emacs-i3-move-window
      (intern (elt (split-string command) 1))))
    ((rx bos "resize")
     (my/emacs-i3-resize-window
      (intern (elt (split-string command) 2))
      (intern (elt (split-string command) 1))
      (string-to-number (elt (split-string command) 3))))
    ("layout toggle stacking tabbed split" (transpose-frame))
    ("split h" (evil-window-split))
    ("split v" (evil-window-vsplit))
    ("kill" (evil-quit))
    (- (i3-msg command))))

;;----------------------------------------------------------------
;; ** qtile integration
;;----------------------------------------------------------------

(defvar qtile-directions-alist
  '(("left"          . left)
    ("right"         . right)
    ("up"            . up)
    ("down"          . down)
    ("shuffle_left"  . left)
    ("shuffle_right" . right)
    ("shuffle_up"    . up)
    ("shuffle_down"  . down)))

(defun qtile-move (&rest args)
  "FIXME"
  (pcase (nth 0 args)
    ("move" (qtile--windmove
             (assoc (nth 1 args) qtile-directions-alist)))
    ("swap" (qtile--swap-window
             (assoc (nth 1 args) qtile-directions-alist)))))

(defmacro qtile-cmd (&rest args)
  `(start-process "emacs-qtile-windmove" nil
    "qtile" "cmd-obj" "-o" "layout" "-f" ,@args))

(defun qtile--windmove (direction)
  "FIXME"
  (let ((other-window (windmove-find-other-window (cdr direction))))
    (if (or (null other-window) (window-minibuffer-p other-window))
        ;; (qtile-cmd (car direction))
        (qtile--net-call "layout" (car direction))
      (windmove-do-window-select (cdr direction)))))

(defun qtile--direction-exists-p (dir)
  (seq-some (lambda (dir)
              (let ((win (windmove-find-other-window dir)))
                (and win (not (window-minibuffer-p win)))))
            (pcase dir
              ('width '(left right))
              ('height '(up down)))))

(defun qtile--swap-window (direction &rest _)
  (let ((other-window (windmove-find-other-window (cdr direction))))
    (cond
     ((and other-window (not (window-minibuffer-p other-window)))
      (window-swap-states (selected-window) other-window))
     (t ;; (qtile-cmd (car direction))
      (qtile--net-call "layout" (car direction))))))


(defvar qtile--socket-dir
  (file-name-concat
   (or (getenv "XDG_CACHE_HOME")
       (expand-file-name "~/.cache/"))
   "qtile/"))

(defvar qtile--socket nil)
(setq qtile--socket
      (let ((f (file-name-concat
                qtile--socket-dir
                (concat "qtilesocket."
                        (or (getenv "DISPLAY")
                            (getenv "WAYLAND_DISPLAY"))))))
        (if-let ((_ (string-match-p ":" f))
                 (f-link (expand-file-name
                          (make-temp-name "qtile-emacs-")
                          temporary-file-directory)))
            ;; (file-name-concat qtile--socket-dir "qtile-emacs")
            (prog1  f-link
              (unless (file-symlink-p f-link)
                (make-symbolic-link f f-link)
                (add-hook 'kill-emacs-hook
                          (lambda ()
                            "Clean up socket connection to Qtile"
                            (delete-file f-link)))))
          f)))

(defun qtile--net-call (object command &rest args)
  (let ((emacs-qtile-proc
         (make-network-process :name "emacs-qtile-proc"
                               :buffer "*qtile-net-proc*"
                               :service qtile--socket
                               :family 'local
                               :coding 'utf-8)))
    (process-send-string
     emacs-qtile-proc
     (concat
      (json-serialize `[[[,object :null]] ,command  [,@args] nil ])
      "\n"))
    ;; (qtile-socat object command)
    (process-send-eof emacs-qtile-proc)))

;;;; Socat process test
;; (defun qtile--ipc-call (object command &rest args)
;;   (let ((emacs-qtile-proc
;;          (make-process :name "emacs-qtile-proc"
;;                        :buffer nil
;;                        :command `("socat" "-"
;;                                   ,(concat "UNIX-CONNECT:"
;;                                     qtile--socket))
;;                        :coding 'utf-8)))
;;     (process-send-string
;;      emacs-qtile-proc
;;      (concat
;;       (json-serialize `[[[,object :null]] ,command  [,@args] nil ])
;;       "\n"))
;;     ;; (qtile-socat object command)
;;     (process-send-eof emacs-qtile-proc)))

;;;; network-process + tq test
;;
;; (tq-enqueue
;;  qtile--queue
;;  (json-serialize [[["layout" :null]] "right" [] nil])
;;  "" nil (lambda (c ans) (prin1 ans (current-buffer))))
;;
;;
;; (defvar qtile--queue
;;   (tq-create
;;    (make-network-process :name "qtile-socket-q"
;;                          :family 'local
;;                          :service "/home/karthik/.cache/qtile/qtiletest")))

;;----------------------------------------------------------------
;; ** niri integration
;;----------------------------------------------------------------

(defvar niri-directions-focus-alist
  '(("focus-column-left"          . left)
    ("focus-column-right"         . right)
    ("focus-window-or-workspace-up"            . up)
    ("focus-window-or-workspace-down"          . down)))

(defvar niri-directions-move-alist
  '(("move-column-left"          . left)
    ("move-column-right"         . right)
    ("move-window-down-or-to-workspace-down"            . up)
    ("move-window-up-or-to-workspace-up"          . down)))

(eval-when-compile
  (defmacro niri-msg-action (&rest args)
    `(start-process "emacs-niri-windmove" nil "niri" "msg" "action" ,@args)))


(defun my/emacs-niri-move-focus (direction-pair)
  "Move focus to the window in the given DIRECTION-PAIR.
DIRECTION-PAIR is a cons cell where car is the niri command and cdr is the Emacs direction.
If there's an Emacs window in that direction, move to it.
Otherwise, send the command to niri."
  (let ((niri-command (car direction-pair))
        (emacs-direction (cdr direction-pair)))
    (let ((other-window (windmove-find-other-window emacs-direction)))
      (if (or (null other-window) (window-minibuffer-p other-window))
          (niri-msg-action niri-command)
        (windmove-do-window-select emacs-direction)))))

(defun my/emacs-niri-move-window (direction-pair &rest _)
  "Move the current window in the given DIRECTION-PAIR.
DIRECTION-PAIR is a cons cell where car is the niri command and cdr is the Emacs direction.
If there's an Emacs window in that direction, swap with it.
Otherwise, send the command to niri."
  (let* ((niri-command (car direction-pair))
         (emacs-direction (cdr direction-pair))
         (other-window (windmove-find-other-window emacs-direction))
         (other-direction (my/emacs-i3-direction-exists-p
                           (pcase emacs-direction
                             ('up 'width)
                             ('down 'width)
                             ('left 'height)
                             ('right 'height)))))
    (cond
     ((and other-window (not (window-minibuffer-p other-window)))
      (window-swap-states (selected-window) other-window))
     (other-direction
      (evil-move-window emacs-direction))
     (t (niri-msg-action niri-command)))))

(defun my/emacs-niri-integration (command)
  "Entry point for niri integration. Dispatches COMMAND to the appropriate function."
  (let ((focus-direction (assoc command niri-directions-focus-alist))
        (move-direction (assoc command niri-directions-move-alist)))
    (cond
     ;; Handle focus commands
     (focus-direction
      (my/emacs-niri-move-focus focus-direction))

     ;; Handle move commands
     (move-direction
      (my/emacs-niri-move-window move-direction))

     ;; Handle close window
     ((string= command "close-window")
      (evil-quit))

     ;; Pass through any other niri command
     (t (niri-msg-action command)))))

(comment

 (my/emacs-niri-integration "focus-column-right")
 (my/emacs-niri-integration "focus-window-or-workspace-down")
 (my/emacs-niri-integration "move-column-right")
 (my/emacs-niri-integration "move-column-left")
 (my/emacs-niri-integration "move-column-right")
 )
;;----------------------------------------------------------------
;; ** hyprland integration
;;----------------------------------------------------------------

(defvar hypr-directions-alist
  '(("l"          . left)
    ("r"         . right)
    ("u"            . up)
    ("d"          . down)))

(eval-when-compile
  (defmacro hyprctl (&rest args)
    `(start-process "emacs-hypr-windmove" nil "hyprctl" "dispatch" ,@args)))


(defun my/emacs-hypr-windmove (direction)
  "Move focus to the window in the given DIRECTION, whether it be a hyprland window or an Emacs window."
  (let ((other-window (windmove-find-other-window (cdr direction))))
    (if (or (null other-window) (window-minibuffer-p other-window))
        (hyprctl "movefocus" (car direction))
      (windmove-do-window-select (cdr direction)))))

(defun my/emacs-hypr-move-window (direction &rest _)
  "Move the current window in the given DIRECTION, whether it be a hyprland window or an Emacs window."
  (let ((other-window (windmove-find-other-window (cdr direction)))
        (other-direction (my/emacs-i3-direction-exists-p
                          (pcase (car direction)
                            ("u" 'width)
                            ("d" 'width)
                            ("l" 'height)
                            ("r" 'height)))))
    (cond
     ((and other-window (not (window-minibuffer-p other-window)))
      (window-swap-states (selected-window) other-window))
     (other-direction
      (evil-move-window (cdr direction)))
     (t (hyprctl "movewindow" (car direction)))
     )))

(comment

 (my/emacs-hypr-move-window
  (assoc "r" hypr-directions-alist)
  ))

(defun my/emacs-hypr-integration (command)
  "Entry point for hyprland integration. Dispatches COMMAND to the appropriate function."
  (let ((args (split-string command)))
    (pcase (car args)
      ("movefocus"
       (my/emacs-hypr-windmove
        (assoc (nth 1 args) hypr-directions-alist)))
      ("movewindow"
       (my/emacs-hypr-move-window
        (assoc (nth 1 args) hypr-directions-alist)))

      ("killactive" (evil-quit))
      (_ (hyprctl command)))))

(comment


 (let ((direction (assoc "r" hypr-directions-alist)))
   (let ((other-window (windmove-find-other-window (cdr direction))))
     ;;(or (null other-window) (window-minibuffer-p other-window))
     (or (null other-window) (window-minibuffer-p other-window))
     other-window
     ))
 (my/emacs-hypr-integration "movefocus r")
 (my/emacs-hypr-integration "movewindow r")
 (assoc (nth 1 '("r")) hypr-directions-alist)

 (my/emacs-hypr-move-window
  (assoc  (nth 1 (split-string "move-window r")) hypr-directions-alist))

 (nth 1
      (split-string "movefocus r")
      )
 (assoc "r" hypr-directions-alist)
 (message "%s"
          (with-temp-buffer
            (call-process "hyprctl" nil t nil "dispatch" "movefocus" "r")
            (buffer-string)))

 (getenv "HYPRLAND_INSTANCE_SIGNATURE")
 (exec-path-from-shell-copy-env "NIRI_SOCKET")
 (shell-command-to-string "env|grep NIRI_SOC")
 (getenv "NIRI_SOCKET")

 ;;
 )
(provide 'emacs-wm)
;;; emacs-wm.el ends here
