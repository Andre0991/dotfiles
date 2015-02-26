(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; helm
;; source: http://tuhdo.github.io/helm-intro.html
(require 'helm)
(require 'helm-config)

;; From Oh-my-emacs (https://github.com/xiaohanyu/oh-my-emacs/commit/34bf80a0fea61ff1112accfb8448a45dafd2204a)
(require 'cl) ; otherwise emacs complains about "case" in the following block
(setq helm-locate-command
      (case system-type
            ('gnu/linux "locate -i -r %s")
            ('berkeley-unix "locate -i %s")
            ('windows-nt "es %s")
            ('darwin "mdfind -name %s %s")
            (t "locate %s")))

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-c h o") 'helm-occur)

(global-set-key (kbd "C-c h g") 'helm-google-suggest)

(helm-mode 1)

;; ido-mode
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

;; start maximized
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Org mode configuration
;; (customize-set-variable org-M-RET-may-split-line (quote ((default))))

;; spaces instead of tabs for identation
(setq-default tab-width 4 indent-tabs-mode nil)
(setq-default c-basic-offset 4 c-default-style "bsd")

;; adapts identation according to the file
;; (package 'dtrt-indent)
;; (dtrt-indent-mode 1)

;; Remove scrollbars, menu bars, and toolbars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; relative-line-numbers
(add-hook 'prog-mode-hook 'relative-line-numbers-mode t)
(add-hook 'prog-mode-hook 'line-number-mode t)
(add-hook 'prog-mode-hook 'column-number-mode t)


;; (global-relative-line-numbers-mode)

;; Org-mode global suggested keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; timestamps in done tasks
(setq org-log-done t)

;; remember cursor position
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; company-mode
;; use company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)

;; smartparens
(smartparens-global-mode t)
(require 'smartparens-config)

;; space as leader.
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

;; enable evil-mode
(evil-mode 1)

;; evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)


;; Key bindings
; Use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(blink-cursor-mode 0)


;; (load-theme 'solarized-dark t)
;; (load-theme 'wombat t)
;; (load-theme 'sanityinc-solarized-dark t)
(load-theme 'zenburn t)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Monaco")))))
