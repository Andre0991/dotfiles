(require 'seq)
(eval-when-compile
  (require 'subr-x))

;; taken from cider - will use it?
(defun cider-symbol-at-point (&optional look-back)
  "Return the name of the symbol at point, otherwise nil.
Ignores the REPL prompt.  If LOOK-BACK is non-nil, move backwards trying to
find a symbol if there isn't one at point.
Does not strip the : from keywords, nor attempt to expand :: auto-resolved
keywords."
  (or (when-let* ((str (thing-at-point 'symbol)))
        (unless (text-property-any 0 (length str) 'field 'cider-repl-prompt str)
          ;; remove font-locking
          (setq str (substring-no-properties str))
          (if (member str '("." ".."))
              str
            ;; Remove prefix quotes, and trailing . from constructors like Record.
            (thread-last str
			 ;; constructors (Foo.)
			 (string-remove-suffix ".")
			 ;; quoted symbols ('sym)
			 (string-remove-prefix "'")
			 ;; var references (#'inc 2)
			 (string-remove-prefix "#'")))))
      (when look-back
        (save-excursion
          (ignore-errors
            (when (looking-at "(")
              (forward-char 1))
            (while (not (looking-at "\\sw\\|\\s_\\|\\`"))
              (forward-sexp -1)))
          (cider-symbol-at-point)))))

(setq cljdocs--export nil)

(defun cljdocs--file-as-string (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun cljdocs--clj-symbol-name (clj-sym)
  (let ((split-clj-symbol (split-string clj-symbol "/")))
    (if (= 1 (length split-clj-symbol))
	(car split-clj-symbol)
      (car (cdr split-clj-symbol)))))

(defun cljdocs--cache-json-export ()
  ;; TODO: optmize - only do that if `cljdocs--export` is not set already
  (setq cljdocs--export (thread-last "~/dev/peric/clojuredocs/clojuredocs-export.json"
				     cljdocs--file-as-string
				     json-parse-string
				     (gethash "vars"))))

(defun cljdocs-filter-candidates
    (name)
  (seq-filter (lambda (el)
		(string= (gethash "name" el) name))
	      cljdocs--export))

(defun cljdocs-summary-candidate
    (var)
  (format "%s/%s"
	  (gethash "ns" var)
	  (gethash "name" var)))

(defun cljsdocs--find-var
    (ns name)
  (car (seq-filter (lambda (el)
		     (and (string= ns (gethash "ns" el))
			  (string= name (gethash "name" el))))
		   cljdocs--export)))

(defun cljdocs--fmt-buffer-content
    (var)
  (let ((examples (seq-map (lambda (el)
			     (gethash "body" el)) (gethash "examples" var))))
    ;; TODO: Use string-join from subr-x.el instead
    ;; see https://stackoverflow.com/questions/12999530/is-there-a-function-that-joins-a-string-into-a-delimited-string
    (string-join examples "\n\n;;;; Example \n\n")))

(defun cljdocs--buffer-name
    (ns name)
  (format "*cljdocs* - %s/%s" ns name))

(defun cljdocs--display
    (var-str)
  (let* ((var-str-split (split-string var-str "/"))
	 (ns (car var-str-split))
	 (name (car (cdr var-str-split))))
    ;; TODO: check if buffer already exists? is it necessary?
    (with-output-to-temp-buffer (cljdocs--buffer-name ns name)
      (princ (cljdocs--fmt-buffer-content (cljsdocs--find-var ns name)))
      (pop-to-buffer (cljdocs--buffer-name ns name))
      (clojure-mode))))


(defun clojuredocs ()
  (interactive)
  ;; TODO: Handle #'foo, 'foo etc? (see cider example) 
  (if-let ((clj-symbol (thing-at-point 'symbol)))
      (progn
	(cljdocs--cache-json-export)
	(let ((candidates (thread-last clj-symbol
				       cljdocs--clj-symbol-name
				       cljdocs-filter-candidates
				       (seq-map #'cljdocs-summary-candidate))))
	  (if (= 1 (length candidates))
	      (cljdocs--display (car candidates))
	    (let ((var (completing-read "var: " (thread-last clj-symbol
							     cljdocs--clj-symbol-name
							     cljdocs-filter-candidates
							     (seq-map #'cljdocs-summary-candidate)))))
	      (cljdocs--display var)))))
    (message "clojuredocs: could not find symbol at point")))

;; (cljdocs--display "clojure.core/filter")
