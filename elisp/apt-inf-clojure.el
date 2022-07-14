;; apt-inf-clojure.el --- Helper functions for `inf-clojure` -*- lexical-binding: t; -*-

;;; Code:

(defvar apt--inf-clojure-all-tests-regex ".*")
(defvar apt--inf-clojure-run-test-test "")
(defvar apt--inf-clojure-eval-form)
(defvar apt--inf-clojure-eval-form-ns)

;;; Helpers

(defun apt--loose-thing-at-point ()
  (let ((symbol-at-point (thing-at-point 'symbol)))
    (if symbol-at-point
	symbol-at-point
      (save-excursion
	(forward-char 1)
	(thing-at-point 'symbol)))))

;;; Inf Clojure

(defun apt-inf-clojure-init-repl ()
  "Sane options for using a repl."
  (inf-clojure-eval-string "(do (require '[clojure.main :as m])
                                (require '[clojure.pprint :as pp])
                                (clojure.main/repl :print pp/pprint
                                                   :init #(do
                                                           ;; The :init hook replicates what clojure.main uses as an initial binding set, so it’s good to repeat that.
                                                           ;; From https://insideclojure.org/2020/02/11/custom-repl/
                                                           (apply require m/repl-requires)
                                                           ;; Workaround for rebinding clojure.test/*test-out*.
                                                           ;; For details, see https://github.com/nrepl/nrepl/blob/a057874cd3bfc83e465cc163fbc1d4c00223e1b1/src/clojure/nrepl/middleware/interruptible_eval.clj#L93-L99\"
                                                           (require 'clojure.test)
                                                           (let [bindings (into (get-thread-bindings)
                                                                                {#'clojure.test/*test-out* *out*})]
                                                             (pop-thread-bindings)
                                                             (push-thread-bindings bindings)))))"))

(defun apt-inf-clojure-run-all-tests ()
  (interactive)
  (let ((cmd (format "(clojure.test/run-all-tests #\"%s\")"
		     apt--inf-clojure-all-tests-regex)))
    (message cmd)
    (inf-clojure-eval-string cmd)))

(defun apt-inf-clojure-run-single-test ()
  (interactive)
  (let ((cmd (format "(clojure.test/run-test %s)"
		     apt--inf-clojure-run-test-test)))
    (message cmd)
    (inf-clojure-eval-string cmd)))

(defun apt-inf-clojure-run-namespaces-tests ()
  (interactive)
  (inf-clojure-eval-string "(clojure.test/run-tests)"))

(defun apt-inf-clojure-set-tests-regex ()
  (interactive)
  (setq apt--inf-clojure-all-tests-regex (read-string "Regex: ")))

(defun apt-inf-clojure-set-single-test ()
  (interactive)
  (setq apt--inf-clojure-run-test-test (read-string "Test: ")))

(defun apt-inf-clojure-run-test-at-point ()
  (interactive)
  (save-excursion
    (inf-clojure-eval-defun)
    ;; for when point is at the very beginning of the defun
    (forward-char 1)
    (beginning-of-defun)
    (end-of-line)
    (inf-clojure-eval-string (format "(%s)"
                                     (symbol-at-point)))))

(defun apt-inf-clojure-repl-name ()
  (let* ((project (project-current))
	(project-name (car (last (split-string (project-root project) "/" 't)))))
    (format "*inf-clojure %s*" project-name)))

(defun apt-inf-clojure-connect (arg)
  (interactive "P")
  (apt-open-in-other-frame
   (lambda ()
     (setq inf-clojure-custom-repl-type 'clojure)
     (cond
      ((not arg) (inf-clojure-connect "localhost" "5555"))
      ((equal '(4) arg) (inf-clojure-connect "localhost" "5556"))
      ((equal '(16) arg) (inf-clojure-connect "localhost" "5557")))
     (apt-inf-clojure-init-repl)
     (apt-inf-clojure-repl-name))))

(defun apt-inf-clojure-source ()
  (interactive)
  (inf-clojure-eval-string (format "(clojure.repl/source %s)"
                                   (apt--loose-thing-at-point))))

(defun apt-inf-clojure-doc ()
  (interactive)
  (inf-clojure-eval-string (format "(clojure.repl/doc %s)"
                                   (apt--loose-thing-at-point))))

(defun apt-inf-clojure-refresh-all ()
  (interactive)
  (inf-clojure-eval-string "(do (require 'clojure.tools.namespace.repl)
                                (clojure.tools.namespace.repl/refresh-all))"))

(defun apt-inf-clojure-refresh ()
  (interactive)
  (inf-clojure-eval-string "(do (require 'clojure.tools.namespace.repl)
                                (clojure.tools.namespace.repl/refresh))"))

(defun apt-inf-clojure-eval-and-replace-last-sexp ()
  (interactive)
  (let* ((start (save-excursion (backward-sexp) (point)))
         (end (point))
         (str (buffer-substring-no-properties start end))
         (str* (replace-regexp-in-string "\n$" "" str)))
    (save-excursion
      (insert (inf-clojure--process-response str (inf-clojure-proc 'no-error))))))

(defun apt-vilpy-describe ()
  (interactive)
  (if (bound-and-true-p inf-clojure-minor-mode)
      (call-interactively 'apt-inf-clojure-doc)
    (call-interactively 'vilpy-describe)))

(defun apt-inf-clojure-add-dependency ()
  (interactive)
  (let* ((dependency (read-string "Dependency: "))
         (version (read-string "Version (default LATEST): " nil nil "LATEST"))
         (clj-expr (format "(require 'cemerick.pomegranate)
 (require 'cemerick.pomegranate.aether)
 (cemerick.pomegranate/add-dependencies :coordinates '[[%s \"%s\"]]
                                        :repositories (merge cemerick.pomegranate.aether/maven-central
                                                             {\"clojars\" \"https://clojars.org/repo\"}
                                                             {\"nu-maven\" \"s3p://nu-maven/releases/\"}))"
                           dependency
                           version)))
    (message clj-expr)
    (inf-clojure-eval-string clj-expr)))

;; taken from https://clojurians-log.clojureverse.org/inf-clojure/2021-03-31
(defun apt-inf-clojure-connect-shadow-repl ()
  (interactive)
  ;; TODO: Add require to deps declaration
  (require 'clojure-mode)
  (setq inf-clojure-custom-repl-type 'cljs)
  (let* ((shadow-file (expand-file-name "socket-repl.port"
                                        (file-name-as-directory
                                         (concat (file-name-as-directory (clojure-project-dir)) ".shadow-cljs"))))
         (shadow-port (when (file-exists-p shadow-file)
                        (with-temp-buffer
                          (insert-file-contents shadow-file)
                          (buffer-substring-no-properties (point-min) (point-max))))))
    (if shadow-port
	(inf-clojure-connect "localhost" shadow-port)
      (message "No shadow socket repl info found"))))

(defun apt-toggle-comint-scroll-to-bottom-on-output
    ()
  (interactive)
  (setq comint-scroll-to-bottom-on-output (if (equal 'others comint-scroll-to-bottom-on-output)
					      (progn (message "Setting `comint-scroll-to-bottom-on-output` to nil")
						     nil)
					    (progn (message "Setting `comint-scroll-to-bottom-on-output` to 'others")
						   'others))))

(defun apt-set-evaluation-form ()
  (interactive)
  (save-excursion
    (mark-sexp)
    (setq apt--inf-clojure-eval-form
	  (buffer-substring (region-beginning)
			    (region-end)))
    (deactivate-mark)
    (setq apt--inf-clojure-eval-form-ns (clojure-find-ns))))

(defun apt-eval-last-set-form ()
  (interactive)
  (let* ((ns-before (clojure-find-ns))
	 (eval-form (format "
(do
  (with-out-str (in-ns '%s))
  (let [res %s]
    (with-out-str (in-ns '%s))
    res))
"
			    apt--inf-clojure-eval-form-ns
			    apt--inf-clojure-eval-form
			    ns-before)))
    (message apt--inf-clojure-eval-form)
    (inf-clojure-eval-string eval-form)))

(with-eval-after-load 'inf-clojure
  (require 'transient)
  (transient-define-prefix apt--vilpy-clojure-prefix ()
    "Displays Clojure-specific commands."
    [["Clojure"
      ("c" "Connect" apt-inf-clojure-connect)
      ("r" "Refresh" apt-inf-clojure-refresh)
      ("i" "Switch to current ns" (lambda () (interactive) (call-interactively 'inf-clojure-set-ns)))
      ("s" "Source" apt-inf-clojure-source)]
     ["Eval"
      ("e s" "Set evaluation expression" apt-set-evaluation-form)
      ("e e" "Eval latest set expression" apt-eval-last-set-form)]
     ["Tests" ("d" "Doc" apt-inf-clojure-doc)
      ("t r" "Run a single test" apt-inf-clojure-run-single-test)
      ("t R" "Set single test" apt-inf-clojure-set-single-test)
      ("t a" "Run all tests" apt-inf-clojure-run-all-tests)
      ("t A" "Set all tests regex" apt-inf-clojure-set-tests-regex)
      ("t t" "Run test at point" apt-inf-clojure-run-test-at-point)
      ("t n" "Run namespace tests" apt-inf-clojure-run-namespaces-tests)]
     ["Repl" ("d" "Doc" apt-inf-clojure-doc)
      ("z" "Switch to repl" inf-clojure-switch-to-repl)
      ("o" "Clear repl" inf-clojure-clear-repl-buffer)
      ("b" "Toggle comint-scroll-to-bottom-on-output" apt-toggle-comint-scroll-to-bottom-on-output)]])

  (with-eval-after-load 'vilpy
    (vilpy-define-key vilpy-mode-map "c" 'apt--vilpy-clojure-prefix)))

(provide 'apt-inf-clojure)

;;; apt-inf-clojure.el ends here
