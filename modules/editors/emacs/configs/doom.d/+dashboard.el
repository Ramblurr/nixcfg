;;; +dashboard.el -*- lexical-binding: t; -*-


(setq +doom-dashboard-functions
  '(doom-dashboard-widget-banner
    ;; doom-dashboard-widget-loaded
    doom-dashboard-widget-shortmenu
    doom-dashboard-widget-footer))

(setq +doom-dashboard-menu-sections
  '(("Reload last session"
    :icon (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
    :when (cond ((require 'persp-mode nil t)
                  (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                ((require 'desktop nil t)
                  (file-exists-p (desktop-full-file-name))))
    :face (:inherit (doom-dashboard-menu-title bold))
    :action doom/quickload-session)
    ("Open Elpher Gemini Browser"
     :icon (all-the-icons-octicon "ruby" :face 'doom-dashboard-menu-title)
     :action elpher)
    ("Open org-agenda"
    :icon (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
    :when (fboundp 'org-agenda)
    :action org-agenda)
    ("Recently opened files"
    :icon (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
    :action recentf-open-files)
    ("Open project"
    :icon (all-the-icons-octicon "briefcase" :face 'doom-dashboard-menu-title)
    :action projectile-switch-project)
    ("Jump to bookmark"
    :icon (all-the-icons-octicon "bookmark" :face 'doom-dashboard-menu-title)
    :action bookmark-jump)
    ("Open private configuration"
    :icon (all-the-icons-octicon "tools" :face 'doom-dashboard-menu-title)
    :when (file-directory-p doom-private-dir)
    :action doom/open-private-config)
    ("Open documentation"
    :icon (all-the-icons-octicon "book" :face 'doom-dashboard-menu-title)
    :action doom/help)))

;; Change the footer icons to go to what I want
(defun doom-dashboard-widget-footer ()
  (insert
   "\n"
   (+doom-dashboard--center
    (- +doom-dashboard--width 2)
    (with-temp-buffer
      (insert-text-button (or (all-the-icons-material "wb_sunny" :face 'doom-dashboard-footer-icon :height 1.3 :v-adjust -0.15)
                              (propertize "wttr" 'face 'doom-dashboard-footer))
                          'action (lambda (_) (browse-url "https://wttr.in/"))
                          'follow-link t
                          'help-echo "Check the weather - wttr.in")
      (buffer-string)))
    "\n"))
