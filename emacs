; -*-Lisp-*-
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Move between emacs windows using super + arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'super))

;; Use (y/n) instead of (yes/no)
(fset 'yes-or-no-p 'y-or-n-p)

;; Bell
;; (setq ring-bell-function 'ignore) ; turn off alarms
(setq visible-bell 1) ; use visual alarm instead of sound


;; backups
;; From http://pages.sachachua.com/.emacs.d/Sacha.html#unnumbered-1
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)    ; do not delete old versions
(setq version-control t)         ; backups are numbered
(setq vc-make-backup-files t)    ; by default, emacs does not backup files managed by a version control system. Setting it to "t" modifies that.
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; alternative to default describe-bindings
(require 'helm-descbinds)
(helm-descbinds-mode)

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

;; relative-line-numbers (from http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/)
(add-hook 'org-mode-hook 'relative-line-numbers-mode t)
(add-hook 'org-mode-hook 'line-number-mode t)
(add-hook 'org-mode-hook 'column-number-mode t)

;; (add-hook 'prog-mode-hook 'relative-line-numbers-mode t)
;; (add-hook 'prog-mode-hook 'line-number-mode t)
;; (add-hook 'prog-mode-hook 'column-number-mode t)

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

;; evil-nerd-commenter
;; Settings according to tip 4 on https://github.com/redguardtoo/evil-nerd-commenter
(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  " "  'evilnc-comment-operator ; Use <SPC> instead of \\
  )

;; evil-numbers
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; enable evil-mode
(evil-mode 1)

;; evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)


;; evil-visualstar
(global-evil-visualstar-mode)

;; yasnippet
(require 'yasnippet)
(yas-reload-all) ; global-mode can affect negatively other modes, use this instead to used it as a non-global minor mode
(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-minor-mode)))

;; Key bindings
; Use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; done in org-mode
(define-key evil-normal-state-map (kbd "<SPC> d") 'org-todo)

;; M-x
(define-key evil-normal-state-map (kbd "<SPC> f") 'helm-M-x)
(define-key evil-visual-state-map (kbd "<SPC> f") 'helm-M-x)

;; C-x C-q
(define-key evil-normal-state-map (kbd "<SPC> q") 'save-buffers-kill-terminal)
(define-key evil-visual-state-map (kbd "<SPC> q") 'save-buffers-kill-terminal)

;; C-x b
(define-key evil-normal-state-map (kbd "<SPC> j") 'helm-mini)
(define-key evil-visual-state-map (kbd "<SPC> j") 'helm-mini)

;; helm-locate
(define-key evil-normal-state-map (kbd "<SPC> l") 'helm-locate)
(define-key evil-visual-state-map (kbd "<SPC> l") 'helm-locate)

;; helm-imenu
(define-key evil-normal-state-map (kbd "<SPC> m") 'helm-imenu)
(define-key evil-visual-state-map (kbd "<SPC> m") 'helm-imenu)

;; save-buffer
(define-key evil-normal-state-map (kbd "<SPC> s") 'save-buffer)
(define-key evil-visual-state-map (kbd "<SPC> s") 'save-buffer)

;; help
(define-key evil-normal-state-map (kbd "<SPC> h") 'help-command)
(define-key evil-visual-state-map (kbd "<SPC> h") 'help-command)

;; other-window
(define-key evil-normal-state-map (kbd "<SPC> k") 'other-window)
(define-key evil-visual-state-map (kbd "<SPC> k") 'other-window)

;; esc quits
;; From https://github.com/davvil/.emacs.d/blob/master/init.el
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(blink-cursor-mode 0)


;; Theme
(if window-system
    (load-theme 'zenburn t)
  (load-theme 'wombat t))

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; wrap lines - visual line mode
;; (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(global-visual-line-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Monaco")))))
