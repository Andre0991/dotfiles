;; apt-mac-os.el --- Mac OS helper functions -*- lexical-binding: t; -*-

;;; Code:

(defun apt-mac-os-dark-mode ()
  (interactive)
  (call-process-shell-command "osascript -l JavaScript -e \"Application('System Events').appearancePreferences.darkMode = true\"" nil 0))

(defun apt-mac-os-light-mode ()
  (interactive)
  (call-process-shell-command "osascript -l JavaScript -e \"Application('System Events').appearancePreferences.darkMode = false\"" nil 0))

(defun apt-mac-os-switch-safari ()
  (interactive)
  (call-process-shell-command "open -a Safari.app" nil 0))

(defun apt-mac-os-switch-slack ()
  (interactive)
  (call-process-shell-command "open -a Slack.app" nil 0))

(provide 'apt-mac-os)

;;; apt-helpers.el ends here
