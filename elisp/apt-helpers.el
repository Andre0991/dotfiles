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
  (find-file "~/.emacs.el"))

(defun apt-pop-to-messages-buffer
    ()
  (interactive)
  (pop-to-buffer "*Messages*"))

(defun apt-backward-and-recenter
    ()
  (interactive)
  (backward-paragraph 1)
  (recenter))

(defun apt-forward-and-recenter
    ()
  (interactive)
  (forward-paragraph 1)
  (recenter))

(defun apt-connect-or-compile
    ()
  (interactive)
  (cond ((or (derived-mode-p 'clojure-ts-mode)
             (derived-mode-p 'clojure-mode))
         (call-interactively #'apt-inf-clojure-connect))
        ((or (derived-mode-p 'go-ts-mode)
             (derived-mode-p 'go-mode))
         (recompile))))

(defun apt-quick-edit-start ()
  "Create a new buffer with a random name and switch to it."
  (interactive)
  (let* ((my-buffer (generate-new-buffer-name (format "random-buffer-%s"
                                                      (format-time-string "%Y%m%d%H%M%S%N")))))
    (generate-new-buffer my-buffer)
    (switch-to-buffer my-buffer)
    (yank)
    (beginning-of-buffer)
    ))

(defun apt-quick-edit-end ()
  "Util function to be executed on qed completion."
  (interactive)
  (mark-whole-buffer)
  (call-interactively 'kill-ring-save)
  (kill-buffer-and-window))

(provide 'apt-helpers)


;;; apt-helpers.el ends here
