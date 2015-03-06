(package-initialize) ; Not using built-in version, otherwise set it to nill

(setq vc-follow-symlinks nil)

(require 'org)
(require 'ob-tangle)

(org-babel-load-file "~/.emacs.d/emacs_andre.org")
