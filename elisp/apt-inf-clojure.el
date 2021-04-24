;; apt-inf-clojure.el --- Helper functions for `inf-clojure` -*- lexical-binding: t; -*-

;;; Code:

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

(defun apt-inf-clojure-doc ()
  (interactive)
  (inf-clojure-eval-string (format "(clojure.repl/doc %s)"
                                   (vilpy--current-function))))

(defun apt-inf-clojure-run-test-at-point ()
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (end-of-line)
    (inf-clojure-eval-string (format "(%s)"
                                     (symbol-at-point)))))

(defun apt-inf-clojure-connect ()
  (interactive)
  (inf-clojure-connect "localhost" "5555")
  (apt-inf-clojure-init-repl))

(defun apt-inf-clojure-source ()
  (interactive)
  (inf-clojure-eval-string (format "(clojure.repl/source %s)"
                                   (vilpy--current-function))))

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

(provide 'apt-inf-clojure)

;;; apt-inf-clojure.el ends here
