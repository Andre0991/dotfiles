;;; Personal configuration -*- lexical-binding: t -*-

;;; Packages

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Install missing packages:
;; (package-install-selected-packages)

;; Remove unused packages:
;; (package-autoremove)

(setq package-selected-packages '(avy
				  clojure-mode
				  code-review
				  consult
				  corfu
				  denote
				  diminish
				  edit-indirect ; for editing blocks in markdown-mode
				  eglot
				  embark
				  embark-consult
				  flymake-shellcheck
				  forge
				  gif-screencast
				  iedit
				  inf-clojure
				  link-hint
				  magit
				  marginalia
				  markdown-mode
				  browse-at-remote
				  md4rd
				  mermaid-mode
				  modus-themes
				  olivetti
				  orderless
				  package-lint
				  pdf-tools
				  sx
				  use-package
				  vertico
				  wgrep
				  yaml-mode))

(setq use-package-enable-imenu-support t
      use-package-compute-statistics t)
(eval-when-compile
  (require 'use-package))
(require 'diminish)

(require 'apt-helpers nil 'noerror)

(use-package emacs
  :init
  (setq initial-frame-alist
	'((top . 1)
	  (left . 1)
	  (fullscreen . fullheight))
	default-frame-alist '((left . (+ 1200))
			      (fullscreen . fullheight)))
  (setq custom-file (make-temp-file "emacs-custom-"))
  (when (string= system-type 'darwin)
    ;; translate super to control
    (setq ns-command-modifier 'control
	  insert-directory-program "/usr/local/bin/gls"))
  (unless (daemonp)
    (advice-add #'display-startup-echo-area-message :override #'ignore))
  (setq vc-follow-symlinks t
	use-short-answers t
	enable-recursive-minibuffers t
	initial-scratch-message nil
	inhibit-startup-screen t
	inhibit-startup-echo-area-message user-login-name
	initial-major-mode 'fundamental-mode
	bookmark-set-fringe-mark nil
	backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/")))
	context-menu-mode t
	completion-cycle-threshold 2
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
  (recentf-mode)

  :bind
  (("M-o" . other-window)
   ("M-O" . other-frame)
   ("M-," . pop-tag-mark)
   ("C-c f d" . delete-file)
   ("C-z" . zap-up-to-char)
   ("M-Z" . zap-up-to-char)
   ("C-S-p" . previous-buffer)
   ("C-S-n" . next-buffer)))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind
  ("C-S-h" . hs-hide-all)
  ("C-S-d" . hs-show-block))

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

;; emacs-mac specific. See https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
(defun apt-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi t))
    ('dark (load-theme 'modus-vivendi t))))
(add-hook 'ns-system-appearance-change-functions #'apt-apply-theme)

(use-package hl-line
  :hook ((prog-mode text-mode) . hl-line-mode))

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
   ("C-S-s" . apt-switch-to-scratch)
   ("C-S-e" . apt-switch-to-emacs-init)))

(use-package dired
  :custom
  ((dired-use-ls-dired t)
   (dired-dwim-target t)))

(use-package elisp-mode
  :diminish emacs-lisp-mode)

(use-package icomplete
  :disabled t
  :init
  (defun apt--enable-truncate-lines
      ()
    "A trivial function that sets `truncate-lines` to t, just
for better naming in the hooks it is listed."
    (setq truncate-lines t))

  (add-hook 'minibuffer-setup-
	    hook
	    #'apt--enable-truncate-lines)
  :custom
  ((icomplete-show-matches-on-no-input t)
   (icomplete-delay-completions-threshold 0)
   (icomplete-max-delay-chars 0)
   (icomplete-compute-delay 0))
  :config
  (icomplete-vertical-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :bind (:map vertico-map
              ("M-V" . #'vertico-multiform-vertical))
  :init
  (vertico-mode))

(use-package vertico-directory
  :after
  vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :after
  vertico
  :init
  (vertico-multiform-mode)
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
	'((consult-imenu buffer indexed)
	  (execute-extended-command unobtrusive))
	vertico-multiform-categories
	'((file grid)
	  (consult-grep buffer))))

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
  :defer t
  :custom 
  (org-confirm-babel-evaluate nil)
  :init
  (add-hook 'org-mode-hook 'variable-pitch-mode))

(use-package markdown-mode
  :custom
  (markdown-fontify-code-blocks-natively t)
  ;; https://github.com/jrblevin/markdown-mode/issues/578
  (markdown-nested-imenu-heading-index nil)
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
  :disabled t
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
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto nil)
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator)))

(use-package consult
  :bind
  (("C-c k" . consult-kmacro)
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("M-y" . consult-yank-pop)
   ("M-s l" . consult-line)
   ("M-s k" . consult-keep-lines)
   ("M-s r" . consult-ripgrep)
   ("M-g f" . consult-flymake)
   ("M-g i" . consult-imenu))
  :init
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format
	xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (defun apt-ripgrep-insert-glob
      ()
    (interactive)
    (insert " -- -g *."))
  (defvar my-consult-ripgrep-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\M-g" #'apt-ripgrep-insert-glob)
      map))
  (consult-customize consult-ripgrep :keymap my-consult-ripgrep-map))


(require 'apt-project-extras nil 'noerror)
(use-package project
  ;; Use this on a new config:
  ;; (project-remember-projects-under "~/dev/nu")
  ;; (project-remember-projects-under "~/dev/peric")
  :bind
  (("C-x p P" . apt-open-project-in-new-tab)
   (:map project-prefix-map
	 ("r" . #'consult-ripgrep)
	 ("t" . apt-project-switch-between-test-and-implementation)))
  :config
  (add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep") t)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  (define-key project-prefix-map "m" #'magit-project-status)
  (define-key project-prefix-map "!" #'project-shell-command)
  (add-to-list 'project-switch-commands '(project-shell-command "Shell command") t))

(use-package embark
  :bind
  (("C-;" . embark-act))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult))

(use-package orderless
  :custom
  (completion-styles '(orderless basic initials))
  (completion-category-overrides '((file (styles basic partial-completion))
				   (project-file (styles basic partial-completion)))))

(use-package vilpy
  :load-path
  "~/dev/peric/vilpy/"
  :hook
  ((emacs-lisp-mode clojure-mode) . vilpy-mode)
  :diminish vilpy-mode)

(use-package clojure-mode
  :custom
  (clojure-align-forms-automatically t)
  :diminish clojure-mode
  :init
  (add-hook 'clojurec-mode-hook (lambda () (diminish 'clojure-mode)))
  (add-hook 'clojurescript-mode-hook (lambda () (diminish 'clojure-mode))))

(use-package eglot
  :init
  (defun apt-eglot-format-if-clojure
      (&rest _)
    (when (and buffer-file-name
	       (derived-mode-p 'clojure-mode)
	       (bound-and-true-p eglot-managed-mode-hook)
	       (bound-and-true-p inf-clojure-minor-mode)
	       (buffer-modified-p))
      (eglot-format)))
  (add-hook 'clojure-mode-hook 'eglot-ensure)
  :custom
  (eglot-confirm-server-initiated-edits nil)
  (eglot-connect-timeout 300)
  :bind
  ("C-c l a" . eglot-code-actions)
  ("C-c l l" . eglot)
  ("C-c l q" . eglot-shutdown)
  ("C-c l r" . eglot-rename)
  ("C-c l u" . xref-find-references)
  :config
  (add-to-list 'eglot-server-programs '(markdown-mode . ("ltex-ls")))
  (dolist (command '(other-window
		     other-frame))
    (advice-add command :before #'apt-eglot-format-if-clojure))
  (add-hook 'eglot-managed-mode-hook
	    ;; This displays full docs for clojure functions.
	    ;; See https://github.com/joaotavora/eglot/discussions/894
	    #'(lambda ()
		(setq-local eldoc-documentation-strategy
			    #'eldoc-documentation-compose

			    eldoc-echo-area-use-multiline-p
			    5))))

(use-package inf-clojure
  :custom
  (inf-clojure-enable-eldoc nil)
  (inf-clojure-mode-line nil)
  :init
  (add-hook 'inf-clojure-mode-hook (lambda () (diminish 'inf-clojure-mode)))
  (add-hook 'inf-clojure-minor-mode-hook (lambda () (diminish 'inf-clojure-minor-mode))))

(require 'apt-inf-clojure nil 'noerror)
(define-key global-map (kbd "C-S-c") #'apt-inf-clojure-connect)

(use-package eww
  :defer t
  :custom
  (eww-search-prefix "https://www.google.com/search?q="))

(use-package browse-at-remote
  :custom
  (browse-at-remote-prefer-symbolic nil)
  :bind
  ("C-c g o" . browse-at-remote))

(require 'apt-smerge-extras nil 'noerror)

(require 'apt-tab-bar-extras nil 'noerror)

(use-package tab-bar
  :bind
  ("C-x t a" . apt-tab-bar-auto-rename-tab)
  ("C-x t k" . tab-bar-close-tab))

(use-package nu-andre
  :load-path
  "~/Dropbox/nu/emacs-lisp"
  :bind
  ("C-S-l" . apt-lein-socket-repl)
  :demand t)

(use-package forge
  :after (magit))

(use-package iedit
  :init
  (with-eval-after-load 'embark
    (define-key embark-general-map (kbd "I") #'iedit-mode)
    (define-key embark-command-map (kbd "I") #'iedit-mode))
  :bind
  ("C-M-;" . iedit-mode) ;; prevents conflict with `embark`
  :custom
  (iedit-toggle-key-default nil))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

(use-package isa
  :if
  (file-directory-p "~/dev/nu/isa.el/")
  :load-path
  "~/dev/nu/isa.el/"
  :bind ("C-c i" . isa))

;;; nu indentation
(let ((nu-clj-indentation-path "~/dev/nu/nudev/ides/emacs/"))
  (when (file-directory-p nu-clj-indentation-path)
    (require 'nu-clj-indentation (expand-file-name "nu-clj-indentation" nu-clj-indentation-path))))
(eval-after-load 'clojure-mode
  '(set-nu-clj-indent))

(use-package howm
  :defer t
  :init
  (setq howm-view-use-grep t
	howm-view-grep-command "rg"
	howm-view-grep-option "-nH --no-heading --color never"
	howm-view-grep-extended-option nil
	howm-view-grep-fixed-option "-F"
	howm-view-grep-expr-option nil
	howm-view-grep-file-stdin-option nil)
  :config
  ;; do not override `C-h`
  (define-key howm-menu-mode-map "\C-h" nil)
  (define-key riffle-summary-mode-map "\C-h" nil)
  (define-key howm-view-contents-mode-map "\C-h" nil))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package eldoc
  :diminish eldoc-mode)

(use-package nuact
  :if (file-directory-p "~/dev/nu/nuact.el")
  :load-path "~/dev/nu/nuact.el"
  :init
  (with-eval-after-load 'vilpy
    (define-key vilpy-mode-map (kbd "<C-return>") #'nuact))
  (with-eval-after-load 'inf-clojure
    (define-key inf-clojure-mode-map (kbd "<C-return>") #'nuact)
    (define-key inf-clojure-minor-mode-map (kbd "<C-return>") #'nuact)))

(use-package md4rd
  :defer t
  :custom
  (md4rd-subs-active '(emacs clojure neovim))
  :config
  (add-hook 'md4rd-mode-hook 'md4rd-indent-all-the-lines))

(use-package code-review
  :after magit
  :custom
  (code-review-auth-login-marker 'forge))

(use-package pdf-tools
  :defer t)

(use-package denote
  :custom
  (denote-directory (expand-file-name "~/denote"))
  (denote-known-keywords '("emacs" "tech"))
  (denote-file-type 'markdown-yaml)
  (denote-dired-directories (list denote-directory))
  :config
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories))
(use-package shr-heading
  :commands
  shr-heading-setup-imenu
  shr-heading-next
  shr-heading-previous)
(use-package eww
  :bind
  (:map eww-mode-map
	("P" . pocket-reader-eww-add-page)
	("{" . backward-paragraph)
	("}" . forward-paragraph)
	("C-c C-p" . shr-heading-previous)
	("C-c C-n" . shr-heading-next))
  :hook (eww-mode . shr-heading-setup-imenu))
(use-package gif-screencast
  ;; on first run, might need to reset permissions:
  ;; https://apple.stackexchange.com/questions/374158/why-is-screencapture-taking-the-screenshot-of-the-desktop-image-and-not-the-wind
  :custom
  (gif-screencast-args '("-x")) ;; To shut up the shutter sound of `screencapture' (see `gif-screencast-command').
  (gif-screencast-cropping-program "mogrify") ;; Optional: Used to crop the capture to the Emacs frame.
  (gif-screencast-capture-format "ppm")
  :bind
  (("<f9>" . gif-screencast-start-or-stop)))

(use-package xwidget
  :bind
  (:map xwidget-webkit-mode-map
	("l" . xwidget-webkit-back)
	("r" . xwidget-webkit-forward)
	("o" . xwidget-webkit-browse-url)
	("r" . xwidget-webkit-reload)))
(use-package link-hint
  :ensure t
  :bind
  ("C-c o" . link-hint-open-link)
  ("C-c o" . link-hint-copy-link))

(use-package sx)
