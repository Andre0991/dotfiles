
;;; Packages

;; Install missing packages with (package-install-selected-packages)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-selected-packages '(modus-themes
				  vertico
				  magit
				  exec-path-from-shell))


;;; Themes

(modus-themes-load-themes)
(modus-themes-load-vivendi)


;;; Emacs
(global-display-line-numbers-mode)


;;; Face
(set-face-attribute 'default nil :height 150)


;;; Vertico
(vertico-mode)


;;; Viper
(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)


;;; Env variables
;; TODO: Make this faster
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

