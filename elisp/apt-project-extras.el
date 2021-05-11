(defun apt-project-switch-between-test-and-implementation ()
    (interactive)
    (cond ((derived-mode-p 'clojure-mode)
	   (let* ((src-dir "/src/")
		  (test-dir "/test/")
		  (test-suffix "_test")
		  (is-test-p (cl-search test-dir buffer-file-name)))
	     (if is-test-p
	       (find-file (format "%s.%s"
                                  (thread-last buffer-file-name
                                               file-name-sans-extension
                                               (string-replace test-dir src-dir)
                                               (replace-regexp-in-string "_test$" ""))
				  (file-name-extension buffer-file-name)))
               (find-file (format "%s%s.%s"
				  (string-replace src-dir test-dir (file-name-sans-extension buffer-file-name))
				  test-suffix
				  (file-name-extension buffer-file-name))))))
	  ((derived-mode-p 'emacs-lisp-mode)
	   (let* ((test-suffix "-test")
		  (is-test-p (cl-search test-suffix buffer-file-name)))
	     (if is-test-p
	       (find-file (format "%s.%s"
                                  (thread-last buffer-file-name
                                               file-name-sans-extension
                                               (replace-regexp-in-string "-test$" ""))
				  (file-name-extension buffer-file-name)))
               (find-file (format "%s%s.%s"
				  (file-name-sans-extension buffer-file-name)
				  test-suffix
				  (file-name-extension buffer-file-name))))))))

(provide 'apt-project-extras)
