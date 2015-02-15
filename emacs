(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; start maximized
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

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

;; relative-line-numbers enabled globally
(add-hook 'prog-mode-hook 'relative-line-numbers-mode t)
(add-hook 'prog-mode-hook 'line-number-mode t)
(add-hook 'prog-mode-hook 'column-number-mode t)

;; Org-mode global suggested keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; timestamps in done tasks
(setq org-log-done t)
;; Required to untick checkboxes (see https://lists.gnu.org/archive/html/emacs-orgmode/2013-11/msg01055.html)
(add-to-list 'load-path "~/.emacs.d/lisp/")
; (require 'org-subtask-reset)
(require 'org-checklist)

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

;; enable evil-mode
(evil-mode 1)

;; " " as leader.
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader " ")

;; Guide-key list - present key bindings for listed prefixes
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(guide-key-mode 1)  ; Enable guide-key-mode

;; Key bindings
; Use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(blink-cursor-mode 0)

;; (load-theme 'tango-dark t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t nil))))
