;; apt-helpers.el --- Helper functions -*- lexical-binding: t; -*-

;;; Code:

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun apt-check-next-def ()
  (push-mark nil t)
  (when (re-search-forward
         (concat "(def\\(?:un\\|macro\\|subst\\|var\\|const\\) "
                 "\\(\\(?:\\sw\\|\\s_\\)+\\)")
         nil 'move)
    (save-excursion
      (let ((name (match-string 1)))
        (goto-char (point-min))
        (unless (re-search-forward (concat "\\_<" name "\\_>") nil t 2)
          name)))))

(defun apt-find-unused-def ()
  (interactive)
  (let (name)
    (while (and (not (eobp))
                (not (setq name (check-next-def)))))
    (message "Found! %s" name)))

;; from https://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun apt-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun apt-switch-to-scratch
    ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun apt-switch-to-emacs-init
    ()
  (interactive)
  (find-file "~/dotfiles/emacs.el"))

(provide 'apt-helpers)

;;; apt-helpers.el ends here
