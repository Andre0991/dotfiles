(defun apt-tab-bar-auto-rename-tab
    ()
  (interactive)
  (when-let (project (project-current))
    (tab-bar-rename-tab (car (last (split-string (project-root project) "/" 't))))))

(provide 'apt-tab-bar-extras)
