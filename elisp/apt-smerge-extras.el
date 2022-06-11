;; apt-smerge-extras.el --- Helper functions for `inf-clojure` -*- lexical-binding: t; -*-

;;; Code:

(provide 'apt-smerge-extras)

;; TODO: Load before smerge?
(with-eval-after-load 'smerge
  (require 'transient)
  (transient-define-prefix apt--smerge-prefix ()
    "Resolve conflicts with smerge"
    :transient-suffix     'transient--do-stay
    [["Navigation"
      ("n" "Next" smerge-next :transient t)
      ("p" "Previous" smerge-prev)]
     ["Conflict"
      ("a" "Keep all" smerge-next)
      ("u" "Keep upper" smerge-keep-upper)
      ("l" "Keep lower" smerge-keep-lower)]
     ["Other"
      ("U" "Undo" undo)]]))

;;; apt-smerge-extras.el ends here
