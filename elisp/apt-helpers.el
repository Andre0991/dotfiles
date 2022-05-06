(defun check-next-def ()
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

(defun find-unused-def ()
  (interactive)
  (let (name)
    (while (and (not (eobp))
                (not (setq name (check-next-def)))))
    (message "Found! %s" name)))

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

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

(global-set-key (kbd "C-c f D")  #'apt-delete-file-and-buffer)



(provide 'apt-helpers)
