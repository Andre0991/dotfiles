;; apt-text-extras.el --- -*- lexical-binding: t; -*-

;;; Code:

;; copied from https://github.com/oantolin/emacs-config/blob/321b2857a8c6eed638c525259a62a21b5434e0dd/my-lisp/text-extras.el#L68
(defun apt-forward-to-whitespace (arg)
  "Move forward to the end of the next sequence of non-whitespace
characters. With argument, do this that many times."
  (interactive "^p")
  (re-search-forward
   (if (> arg 0)
       "[^[:blank:]\n]\\(?:[[:blank:]\n]\\|\\'\\)"
     "\\(?:[[:blank:]\n]\\|\\`\\)[^[:blank:]\n]")
   nil t arg)
  (unless (= (point) (if (> arg 0) (point-max) (point-min)))
    (forward-char (if (> arg 0) -1 1))))

(provide 'apt-text-extras)

;;; apt-text-extras.el ends here
