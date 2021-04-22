
;;; Packages

;; Install missing packages with (package-install-selected-packages)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-selected-packages '(modus-themes
				  vertico
				  magit
				  exec-path-from-shell
				  consult
				  orderless))



;;; Emacs
(global-display-line-numbers-mode)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq vc-follow-symlinks t)
(tool-bar-mode -1)


;;; Themes
(modus-themes-load-themes)
(modus-themes-load-vivendi)


;;; Face
(set-face-attribute 'default nil :height 150)


;;; Vertico
(vertico-mode)


;;; Orderless
(require 'orderless)
(setq completion-styles '(orderless))


;;; Viper
(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)


;;; Env variables
;; TODO: Make this faster
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

