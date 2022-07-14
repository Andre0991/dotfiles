;;; Personal configuration -*- lexical-binding: t -*-

;;; Packages

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Install missing packages:
;; (package-install-selected-packages)

;; Remove unused packages:
;; (package-autoremove)

(setq package-selected-packages '(auto-dark
				  avy
				  browse-at-remote
				  clojure-mode
				  consult
				  corfu
				  diminish
				  eglot
				  embark
				  embark-consult
				  edit-indirect ; for editing blocks in markdown-mode
				  flymake-shellcheck
				  forge
				  iedit
				  inf-clojure
				  magit
				  marginalia
				  markdown-mode
				  mermaid-mode
				  modus-themes
				  olivetti
				  orderless
				  package-lint
				  use-package
				  wgrep
				  which-key
				  yaml-mode))

(setq use-package-enable-imenu-support t)
(eval-when-compile
  (require 'use-package))
(require 'diminish)

;; `emacs` a valid value because (featurep 'emacs) is t
(use-package emacs
  :init
  (setq custom-file (make-temp-file "emacs-custom-"))
  (when (string= system-type 'darwin)
    ;; translate super to control
    (setq ns-command-modifier 'control
	  insert-directory-program "/usr/local/bin/gls"))
  (unless (daemonp)
    (advice-add #'display-startup-echo-area-message :override #'ignore))
  (setq vc-follow-symlinks t
	use-short-answers t
	recentf-mode t
	enable-recursive-minibuffers t
	initial-scratch-message nil
	inhibit-startup-screen t
	inhibit-startup-echo-area-message user-login-name
	initial-major-mode 'fundamental-mode
	bookmark-set-fringe-mark nil
	backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/")))
	context-menu-mode t
	;; Enable indentation+completion using the TAB key.
	;; Completion is often bound to M-TAB.
	tab-always-indent 'complete
	;; completions-detailed t
	compilation-scroll-output 't
	sentence-end-double-space nil
	auth-sources '("~/.authinfo")
	isearch-lazy-count t
	async-shell-command-buffer 'new-buffer)
  (dolist (cmd '(narrow-to-region
		 upcase-region
		 downcase-region
		 narrow-to-page
		 scroll-left
		 scroll-right))
    (put cmd 'disabled nil))
  ;; triggered automatically on long *files*
  ;; (not on text inserted in a buffer)
  (global-so-long-mode)
  ;; lock file, backup file
  (let ((aux-dir "~/.emacs.d/aux"))
    (unless (file-exists-p aux-dir)
      (make-directory aux-dir)))
  (setq lock-file-name-transforms '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t))
	auto-save-file-name-transforms '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t))
	backup-directory-alist '((".*" . "~/.emacs.d/aux/")))
  (set-face-attribute 'default nil :height 190 :family "DejaVu Sans Mono")
  (set-face-attribute 'variable-pitch nil :family "Helvetica" :height 210)

  :bind
  (("M-o" . other-window)
   ("M-O" . other-frame)
   ("M-," . pop-tag-mark)
   ("C-c f d" . delete-file)
   ("M-Z" . zap-up-to-char)))

(use-package modus-themes
  ;; modus' variables need to be set *before* the package is loaded
  ;; change require reloading the theme:
  ;; (modus-themes-load-vivendi)
  :custom
  (modus-themes-italic-constructs nil)
  (modus-themes-bold-constructs nil)
  (modus-themes-tabs-accented nil)
  (modus-themes-mode-line '(accented borderless (padding . 4) (height . 0.9)))
  (modus-themes-hl-line nil)
  (modus-themes-paren-match nil)
  (modus-themes-links '(neutral-underline background))
  (modus-themes-completions '((matches . (extrabold))
			      (selection . (semibold accented))
			      (popup . (accented intense))))
  (modus-themes-region '(accented)))

(use-package hl-line
  :hook ((prog-mode text-mode) . hl-line-mode))

(use-package auto-dark
  :if
  (string= system-type 'darwin)
  :custom
  (auto-dark--light-theme 'modus-operandi)
  (auto-dark--dark-theme 'modus-vivendi)
  :config
  (require 'auto-dark))

(use-package tool-bar
  :if
  (display-graphic-p)
  :config
  (tool-bar-mode -1))

(use-package apt-helpers
  :load-path
  ;; TODO: elisp brings more than apt-helpers 
  "~/dotfiles/elisp/"
  :bind
  (("C-c f D" . apt-delete-file-and-buffer)
   ("C-S-s" . apt-switch-to-scratch)))

;;; TODO: remove me after use-package migration
;; (comment
;;  (dolist (path '("~/dotfiles/elisp/"
;; 		 "~/Dropbox/nu/emacs-lisp"))
;;    (when (file-directory-p path)
;;      (add-to-list 'load-path path))))

(use-package dired
  :custom
  ((dired-use-ls-dired t)
   (dired-dwim-target t)))

(use-package elisp-mode
  :diminish emacs-lisp-mode)

;;; Completion
(defun apt--enable-truncate-lines
    ()
  "A trivial function that sets `truncate-lines` to t, just
for better naming in the hooks it is listed."
  (setq truncate-lines t))

(add-hook 'minibuffer-setup-hook
	  #'apt--enable-truncate-lines)

;; Tab cycle if there are only few candidates
(setq completion-cycle-threshold 2)

(use-package icomplete
  :custom
  ((icomplete-show-matches-on-no-input t)
   (icomplete-delay-completions-threshold 0)
   (icomplete-max-delay-chars 0)
   (icomplete-compute-delay 0))
  :config
  (icomplete-vertical-mode))

(use-package comint
  :custom
  ((comint-move-point-for-output 'others)
   (comint-input-ignoredups t)))

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package flymake
  :bind
  (:map flymake-mode-map
	("M-n" . flymake-goto-next-error)
	("M-p" . flymake-goto-prev-error)))

(use-package pulse
  :init
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (dolist (command '(scroll-up-command
		     scroll-down-command
                     recenter-top-bottom
		     other-window
		     other-frame))
    (advice-add command :after #'pulse-line)))

(use-package elec-pair
  :init
  (defun apt-inhibit-electric-pair-mode (_char)
    ;; do not use electrict pair mode in minifuffer, as
    ;; it is common to use a single parens for searching
    (minibufferp))
  (setq electric-pair-inhibit-predicate #'apt-inhibit-electric-pair-mode)
  :config
  (electric-pair-mode))

(use-package org
  :init
  (add-hook 'org-mode-hook 'variable-pitch-mode))

(use-package markdown-mode
  :custom
  (markdown-fontify-code-blocks-natively t)
  :init
  (add-hook 'markdown-mode-hook 'variable-pitch-mode)
  :config
  (dolist (face '(markdown-inline-code-face
		  markdown-code-face
		  markdown-table-face))
    (set-face-attribute face nil :height 190 :family "DejaVu Sans Mono")))

(use-package winner
  :init
  (add-hook 'after-init-hook #'winner-mode))

(use-package which-key
  :custom
  ((which-key-show-early-on-C-h t)
   ;; make sure which-key doesn't show automatically,
   ;; but refreshes quickly after it is triggered.
   (which-key-idle-delay 10000)
   (which-key-idle-secondary-delay 0.05))
  :config
  (which-key-mode)
  :diminish which-key-mode)

(use-package marginalia
  :init
  (defun marginalia-use-builtin ()
    (mapc
     (lambda (x)
       (setcdr x (cons 'builtin (remq 'builtin (cdr x)))))
     marginalia-annotator-registry))
  :bind
  (:map minibuffer-local-map
	("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  (marginalia-use-builtin))

(use-package corfu
  :custom
  ((corfu-auto nil))
  :hook
  ((prog-mode . corfu-mode)
   (shell-mode . corfu-mode)
   (eshell-mode . corfu-mode))
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator)))

;;; Consult
(setq consult-project-root-function
      (lambda ()
        (when-let (project (project-current))
          (project-root project)))
      xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)
(let ((map global-map))
  (define-key map (kbd "C-c k") #'consult-kmacro)
  (define-key map (kbd "C-x b") #'consult-buffer)
  (define-key map (kbd "C-x 4 b") #'consult-buffer-other-window)
  (define-key map (kbd "M-y") #'consult-yank-pop)
  (define-key map (kbd "M-s l") #'consult-line)
  (define-key map (kbd "M-s k") #'consult-keep-lines)
  (define-key map (kbd "M-s r") #'consult-ripgrep)
  ;; goto-map
  (define-key map (kbd "M-g f") #'consult-flymake)
  (define-key map (kbd "M-g i") #'consult-imenu))
(with-eval-after-load 'project
  (define-key project-prefix-map "r" #'consult-ripgrep)
  (add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep") t))

;;; Embark
(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)))
(define-key global-map (kbd "C-;") #'embark-act)

;;; Orderless
(require 'orderless)
(setq completion-styles '(orderless initials basic)
      completion-category-overrides '((file (styles basic partial-completion))
				      (project-file (styles basic partial-completion))))

;;; Vilpy
(let ((vilpy-path "~/dev/peric/vilpy/"))
  (add-to-list 'load-path vilpy-path)
  (require 'vilpy))
(add-hook 'emacs-lisp-mode-hook (lambda () (vilpy-mode 1)))
(add-hook 'clojure-mode-hook (lambda () (vilpy-mode 1)))
(add-hook 'vilpy-mode-hook (lambda () (diminish 'vilpy-mode)))

;;; Clojure
(setq clojure-align-forms-automatically t)
(add-hook 'clojure-mode-hook (lambda () (diminish 'clojure-mode)))
(add-hook 'clojurec-mode-hook (lambda () (diminish 'clojure-mode)))
(add-hook 'clojurescript-mode-hook (lambda () (diminish 'clojure-mode)))

;;; Eglot
;; TODO: move to eglot extras file
(defun format-if-clojure
    (&rest _)
  (when (and buffer-file-name
	     (derived-mode-p 'clojure-mode)
	     (bound-and-true-p eglot-managed-mode-hook)
	     (bound-and-true-p inf-clojure-minor-mode)
	     (buffer-modified-p))
    (eglot-format)))

(setq eglot-confirm-server-initiated-edits nil)
(setq eglot-connect-timeout 300)
(add-hook 'clojure-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c l a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l l") #'eglot)
  (define-key eglot-mode-map (kbd "C-c l q") #'eglot-shutdown)
  (define-key eglot-mode-map (kbd "C-c l r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l u") #'xref-find-references)
  (dolist (command '(other-window
		     other-frame))
    (advice-add command :before #'format-if-clojure)))
(add-hook 'eglot-managed-mode-hook
	  ;; This displays full docs for clojure functions.
	  ;; See https://github.com/joaotavora/eglot/discussions/894
	  #'(lambda ()
	      (setq-local eldoc-documentation-strategy
			  #'eldoc-documentation-compose

			  eldoc-echo-area-use-multiline-p
			  5)))


;;; ltex-ls
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(markdown-mode . ("ltex-ls"))))

;;; inf-clojure
;; from `elisp-path`
(setq inf-clojure-enable-eldoc nil
      inf-clojure-mode-line nil)
(add-hook 'inf-clojure-mode-hook (lambda () (diminish 'inf-clojure-mode)))
(add-hook 'inf-clojure-minor-mode-hook (lambda () (diminish 'inf-clojure-minor-mode)))
(require 'apt-inf-clojure nil 'noerror)
(define-key global-map (kbd "C-S-c") #'apt-inf-clojure-connect)

;;; eww
(setq eww-search-prefix "https://www.google.com/search?q=")

;;; browse-at-remote
(global-set-key (kbd "C-c g o") 'browse-at-remote)
(setq browse-at-remote-prefer-symbolic nil)

;;; project
;; (project-remember-projects-under "~/dev/nu")
;; (project-remember-projects-under "~/dev/peric")
(require 'apt-project-extras nil 'noerror)
(define-key project-prefix-map (kbd "t") 'apt-project-switch-between-test-and-implementation)
(define-key global-map (kbd "C-x p P") #'apt-open-project-in-new-tab)
(with-eval-after-load 'project
  (define-key project-prefix-map "!" #'project-shell-command)
  (add-to-list 'project-switch-commands '(project-shell-command "Shell command") t))
;; (with-eval-after-load 'consult
;;   (add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep" ?r))∑)

;; smerge
(require 'apt-smerge-extras nil 'noerror)

;;; tab bar
(require 'apt-tab-bar-extras nil 'noerror)
(define-key global-map (kbd "C-x t a") #'apt-tab-bar-auto-rename-tab)
(define-key global-map (kbd "C-x t k") #'tab-bar-close-tab)

;;; Helpers
(require 'apt-helpers nil 'noerror)
(define-key global-map (kbd "C-S-l") #'apt-lein-socket-repl)

;;; Nu
(require 'nu-andre nil 'no-error)

;;; Magit
(with-eval-after-load 'project
  (define-key project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

;;; Forge
(with-eval-after-load 'magit
  (require 'forge))

;;; iedit
;; prevents conflict with `embark`
(setq iedit-toggle-key-default nil)
(define-key global-map (kbd "C-M-;") #'iedit-mode)
(with-eval-after-load 'embark
  (define-key embark-general-map (kbd "I") #'iedit-mode))

;;; wgrep
(require 'wgrep)
(setq wgrep-auto-save-buffer t)

;;; org
(setq org-confirm-babel-evaluate nil)

;;; isa
(let ((isa-path "~/dev/nu/isa.el/"))
  (when (file-directory-p isa-path)
    (add-to-list 'load-path isa-path)
    (require 'isa)
    (define-key global-map (kbd "C-c i") #'isa)))

;;; nu indentation
(let ((nu-clj-indentation-path "~/dev/nu/nudev/ides/emacs/"))
  (when (file-directory-p nu-clj-indentation-path)
    (require 'nu-clj-indentation (expand-file-name "nu-clj-indentation" nu-clj-indentation-path))))
(eval-after-load 'clojure-mode
  '(set-nu-clj-indent))

;;; howm
(require 'howm)
;; use `rg`
(setq howm-view-use-grep t
      howm-view-grep-command "rg"
      howm-view-grep-option "-nH --no-heading --color never"
      howm-view-grep-extended-option nil
      howm-view-grep-fixed-option "-F"
      howm-view-grep-expr-option nil
      howm-view-grep-file-stdin-option nil)
;; do not override `C-h`
(define-key howm-menu-mode-map "\C-h" nil)
(define-key riffle-summary-mode-map "\C-h" nil)
(define-key howm-view-contents-mode-map "\C-h" nil)

;;; auto-revert-mode
(add-hook 'auto-revert-mode-hook (lambda () (diminish 'auto-revert-mode)))

;;; Eldoc
(add-hook 'eldoc-mode-hook (lambda () (diminish 'eldoc-mode)))

;;; nuact
(let ((nuact-path "~/dev/nu/nuact.el"))
  (when (file-directory-p nuact-path)
    (add-to-list 'load-path nuact-path)
    (require 'nuact)
    ;; TODO: C-return ∫depend on GUI/CLI? https://emacs.stackexchange.com/questions/31375/how-can-i-bind-c-return-with-define-key-and-kbd
    (with-eval-after-load 'vilpy
      (define-key vilpy-mode-map (kbd "<C-return>") #'nuact))
    (with-eval-after-load 'inf-clojure
      (define-key inf-clojure-mode-map (kbd "<C-return>") #'nuact)
      (define-key inf-clojure-minor-mode-map (kbd "<C-return>") #'nuact))))
