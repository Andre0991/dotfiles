;;; Personal configuration -*- lexical-binding: t -*-

;;; Packages
(require 'package)
(dolist (archive '(("melpa" . "https://melpa.org/packages/")
                   ("elpa-devel" . "https://elpa.gnu.org/devel/")
                   ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (add-to-list 'package-archives archive))

;; Refresh packages
;; (package-refresh-contents)

;; Install missing packages:
;; (package-install-selected-packages)

;; Remove unused packages:
;; (package-autoremove)

(setq package-selected-packages '(avy
                                  browse-at-remote
                                  ;; surround  ; it bugs meow
                                  ;; eglot-java
                                  cape
                                  clojure-ts-mode
                                  consult
                                  corfu
                                  cue-mode
                                  denote
                                  diminish
                                  eat
                                  edit-indirect ; for editing blocks in markdown-mode
                                  eglot
                                  ellama
                                  embark
                                  embark-consult
                                  engine-mode
                                  evil
                                  flymake-shellcheck
                                  forge
                                  gif-screencast
                                  go-mode
                                  graphviz-dot-mode
                                  grip-mode
                                  iedit
                                  inf-clojure
                                  jarchive
                                  jinx
                                  json-mode
                                  keycast
                                  key-chord
                                  link-hint
                                  magit
                                  marginalia
                                  markdown-mode
                                  md4rd
                                  meow
                                  mermaid-mode
                                  modus-themes
                                  nov
                                  olivetti
                                  orderless
                                  org-download
                                  org-modern
                                  ox-gfm
                                  ox-slack
                                  package-lint
                                  pdf-tools
                                  plz
                                  sly
                                  sx
                                  use-package
                                  vertico
                                  wgrep
                                  yaml-mode
                                  yaml-pro))

(setq use-package-enable-imenu-support t
      use-package-compute-statistics t
      apt-profile (if (file-directory-p "~/dev/nu/") 'work 'home))
(eval-when-compile
  (require 'use-package))
(require 'diminish)

(require 'apt-helpers nil 'noerror)
(require 'apt-mac-os nil 'noerror)

(use-package emacs
  :init
  (when (string= system-type 'darwin)
    ;; translate super to control
    (setq ns-command-modifier 'control
          insert-directory-program "/usr/local/bin/gls"))
  ;; start the initial frame maximized
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  ;; start every frame maximized
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq custom-file (make-temp-file "emacs-custom-"))
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
        context-menu-mode t
        completion-cycle-threshold 2
        ;; Enable indentation+completion using the TAB key.
        ;; Completion is often bound to M-TAB.
        tab-always-indent 'complete
        ;; completions-detailed t
        compilation-scroll-output 't
        sentence-end-double-space nil
        auth-sources '("~/.authinfo.gpg")
        isearch-lazy-count t
        async-shell-command-buffer 'new-buffer
        ;; save buffers in project context (instead of all buffers)
        save-some-buffers-default-predicate #'save-some-buffers-root)
  ;; personal
  (setq apt-narrow-screen t)
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
  (let ((backup-dir "~/.emacs.d/backups"))
    (unless (file-exists-p backup-dir)
      (make-directory backup-dir)))
  (setq-default lock-file-name-transforms '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/backups/\\1" t))
                auto-save-file-name-transforms '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/backups/\\1" t))
                backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
                version-control t
                backup-by-copying t
                vc-make-backup-files t
                kept-new-versions 5
                kept-new-versions 3
                delete-old-versions t
                auto-save-default t
                auto-save-timeout 20
                auto-save-interval 200
                ring-bell-function #'ignore)
  (setq-default indent-tabs-mode nil  ; Don't use tabs for indentation
                tab-width 4
                c-basic-offset 4)

  ;; faces
  ;; DejaVu is great, but it has this bug:
  ;; https://old.reddit.com/r/emacs/comments/3wmw8z/dejavu_sans_mono_line_height_bug/
  ;; (set-face-attribute 'default nil :height 260 :family "DejaVu Sans Mono")

  ;; from https://git.sr.ht/~protesilaos/iosevka-comfy
  ;; (defun font-exists-p (font)
  ;;   "check if font exists"
  ;;   (if (null (x-list-fonts font)) nil t))

  ;; brew install homebrew/cask-fonts/font-iosevka-comfyx
  (set-face-attribute 'default nil :height 240 :family "Iosevka Comfy")
  (set-face-attribute 'variable-pitch nil :family "Helvetica" :height 240)
  (recentf-mode)
  (define-minor-mode apt-sticky-buffer-mode
    "Make the current window always display this buffer."
    :init-value nil
    :lighter " sticky"
    nil
    (set-window-dedicated-p (selected-window) apt-sticky-buffer-mode))
  
  :bind
  (("M-o" . other-window)
   ("M-O" . other-frame)
   ("M-," . pop-tag-mark)
   ("C-c f d" . delete-file)
   ("C-z" . zap-up-to-char)
   ("M-Z" . zap-up-to-char)
   ("C-S-p" . previous-buffer)
   ("C-S-n" . next-buffer)
   ("C-h" . delete-backward-char)
   ("M-g s" . sh-send-line-or-region-and-step)
   ("C-S-r" . raise-sexp)
   ("C-S-z" . repeat)))

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
  (modus-themes-headings
   '((1 . (variable-pitch 1.7))
     (2 . (variable-pitch 1.5))
     (3 . (variable-pitch 1.3))
     (4 . (variable-pitch 1))
     (5 . (variable-pitch 1))
     (6 . (variable-pitch 1))
     (7 . (variable-pitch 1))
     (8 . (variable-pitch 1))
     (t . (monochrome))))
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
  "~/dev/peric/dotfiles/elisp/"
  :bind
  (("C-c f D" . apt-delete-file-and-buffer)
   ("C-S-c" . apt-connect-or-compile)
   ("C-S-s" . apt-switch-to-scratch)
   ("C-S-e" . apt-switch-to-emacs-init)
   ("C-S-m" . apt-pop-to-messages-buffer)))

(use-package apt-mac-os
  :load-path
  "~/dev/peric/dotfiles/elisp/apt-mac-os.el")

(use-package apt-text-extras
  :load-path
  "~/dev/peric/dotfiles/elisp/apt-text-extras.el"
  :bind
  (("C-S-w" . apt-forward-to-whitespace)))

(use-package dired
  :custom
  ((dired-use-ls-dired t)
   (dired-dwim-target t))
  :bind (:map dired-mode-map
              ("C-o" . consult-buffer)
              ("C-c C-o" . dired-display-file)))

(use-package elisp-mode
  :diminish emacs-lisp-mode)

(use-package nuai
  :if
  (file-directory-p "~/dev/nu/nuai.el/")
  :load-path
  "~/dev/nu/nuai.el/"
  :bind ("C-c a" . nuai))

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
              ("C-l" . #'vertico-multiform-vertical))
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

(use-package markdown-mode
  :defer t
  :custom
  (markdown-fontify-code-blocks-natively t)
  ;; https://github.com/jrblevin/markdown-mode/issues/578
  (markdown-nested-imenu-heading-index nil)
  :init
  ;; (add-hook 'markdown-mode-hook 'variable-pitch-mode)
  ;; (add-hook 'markdown-mode-hook
  ;;           (lambda ()
  ;;             (dolist (face '(markdown-inline-code-face
  ;;   		                  markdown-code-face
  ;;   		                  markdown-table-face))
  ;;   	        (set-face-attribute 'default nil :height 190 :family "Iosevka Comfy"))))
  )

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
   ("C-o" . consult-buffer)
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
	    xref-show-definitions-function #'consult-xref
        consult-preview-excluded-files '(".*\\.org$"))
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
  :bind
  (("C-x p P" . apt-open-project-in-new-tab)
   (:map project-prefix-map
	     ("r" . #'consult-ripgrep)
	     ("t" . apt-project-switch-between-test-and-implementation)))
  :config
  ;; Use this on a new config:
  ;; (project-remember-projects-under "/Users/andre.peric/dev/nu/")
  ;; (project-remember-projects-under "~/dev/peric")
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
  ;; TODO: Use add-to-list instead?
  ;; (https://github.com/joaotavora/eglot/issues/131)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vilpy
  ;; mkdir -p ~/dev/peric && cd ~/dev/peric && git clone https://github.com/Andre0991/vilpy.git
  :load-path
  "~/dev/peric/vilpy/"
  :hook
  ((emacs-lisp-mode
    clojure-mode
    clojure-ts-mode
    sly-mode) . vilpy-mode)
  :diminish vilpy-mode)

(use-package breadcrumb
  :disabled t
  ;; mkdir -p ~/dev/peric && cd ~/dev/peric && git clone https://github.com/joaotavora/breadcrumb.git
  :load-path
  "~/dev/peric/breadcrumb/"
  :hook
  ((clojure-mode
    clojure-ts-mode
    sly-mode) . breadcrumb-local-mode)
  :diminish breadcrumb-local-mode)

(use-package consult-gh
  ;; mkdir -p ~/dev/peric && cd ~/dev/peric && git clone https://github.com/armindarvish/consult-gh.git
  :load-path
  "~/dev/peric/consult-gh/"
  :custom
  (consult-gh-default-clone-directory "~/dev/nu")
  (consult-gh-repo-maxnum 999)
  (consult-gh-code-maxnum 100)
  :config
  (add-to-list 'consult-gh-default-orgs-list "Nubank")
  (require 'consult-gh-embark)
  :diminish consult-gh-mode)

(use-package clojure-mode
  :disabled t
  :custom
  (clojure-align-forms-automatically t)
  :diminish clojure-mode
  :init
  (add-hook 'clojurec-mode-hook (lambda () (diminish 'clojure-mode)))
  (add-hook 'clojurescript-mode-hook (lambda () (diminish 'clojure-mode))))

(use-package clojure-ts-mode
  :hook (clojure-ts-mode . (lambda ()
                             ;; replicating clojure-mode settings
                             (setq-local comment-add 1)
                             (setq-local indent-tabs-mode nil
                                         parse-sexp-ignore-comments t
                                         multibyte-syntax-as-symbol t
                                         open-paren-in-column-0-is-defun-start nil
                                         fill-paragraph-function #'clojure-fill-paragraph
                                         adaptive-fill-function #'clojure-adaptive-fill-function
                                         normal-auto-fill-function #'clojure-auto-fill-function

                                         ;; CLOJURE INDENT
                                         indent-line-function #'clojure-indent-line
                                         indent-region-function #'clojure-indent-region
                                         aggressive-indent-region-function #'clojure-indent-region
                                         lisp-indent-function #'clojure-indent-function

                                         lisp-doc-string-elt-property 'clojure-doc-string-elt
                                         clojure-expected-ns-function #'clojure-expected-ns
                                         add-log-current-defun-function #'clojure-current-defun-name
                                         beginning-of-defun-function #'clojure-beginning-of-defun-function))))

(use-package eglot
  ;; upgrade (required once, then it's updated with other packages):
  ;; (eglot-upgrade-eglot)
  :init
  (setq apt-eldoc-echo-area-use-multiline-p (if apt-narrow-screen
                                                nil
                                              5))
  (defun apt-eglot-hooks
      ()
    ;; This displays full docs for clojure functions.
    ;; See https://github.com/joaotavora/eglot/discussions/894
    (setq-local eldoc-documentation-strategy
                #'eldoc-documentation-compose

                eldoc-echo-area-use-multiline-p
                apt-eldoc-echo-area-use-multiline-p))
  (defun apt-eglot-format-if-clojure
      (&rest _)
    (when (and buffer-file-name
	           (derived-mode-p 'clojure-mode)
	           (bound-and-true-p eglot-managed-mode-hook)
	           (bound-and-true-p inf-clojure-minor-mode)
	           (buffer-modified-p))
      (eglot-format)))
  (dolist (mode '(clojure-ts-mode-hook
                  clojure-mode-hook
                  go-mode-hook
                  go-ts-mode-hook))
    (add-hook mode 'eglot-ensure))
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
  (add-to-list 'eglot-server-programs '(org-mode . ("ltex-ls")))
  (dolist (command '(other-window
		             other-frame))
    ;; TODO: might timeout and make Emacs lag
    ;; (advice-add command :before #'apt-eglot-format-if-clojure)
    )
  (add-hook 'eglot-managed-mode-hook
	        ;; This displays full docs for clojure functions.
	        ;; See https://github.com/joaotavora/eglot/discussions/894
	        #'apt-eglot-hooks))

(use-package jarchive
  :config
  (jarchive-setup)
  :after (eglot))

(use-package inf-clojure
  :custom
  (inf-clojure-enable-eldoc nil)
  (inf-clojure-mode-line nil)
  :init
  (add-hook 'inf-clojure-mode-hook (lambda () (diminish 'inf-clojure-mode)))
  ;; (add-hook 'inf-clojure-mode-hook (lambda () (sticky-buffer-mode)))
  (add-hook 'inf-clojure-minor-mode-hook (lambda () (diminish 'inf-clojure-minor-mode)))
  :load-path
  "~/dev/peric/inf-clojure")

(require 'apt-inf-clojure nil 'noerror)
(define-key global-map (kbd "C-S-b") #'apt-inf-clojure-open-bb)

(use-package browse-at-remote
  :custom
  (browse-at-remote-prefer-symbolic nil)
  :bind
  ("C-c g o" . browse-at-remote-kill)
  ("C-c g O" . browse-at-remote))

(require 'apt-smerge-extras nil 'noerror)

(require 'apt-tab-bar-extras nil 'noerror)

(use-package tab-bar
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  :bind
  ("C-x t c" . tab-new)
  ("C-x t a" . apt-tab-bar-auto-rename-tab)
  ("C-x t k" . tab-bar-close-tab)
  ("C-x t n" . tab-bar-new-tab)
  ("C-x t r" . tab-rename))

(use-package nu-andre
  :load-path
  "~/Dropbox/nu/emacs-lisp"
  :bind
  ("C-S-l" . apt-lein-socket-repl)
  :demand t)

(use-package forge
  ;; initial configuration:
  ;; https://gist.github.com/erikmd/8f1e892abb997fcb63ea2359137f4ae9
  ;; and
  ;; https://practical.li/spacemacs/source-control/forge-configuration/
  ;; (note: remember to enable SSO for the created token)
  :after (magit)
  :config
  ;; Display only unread
  ;; https://github.com/magit/forge/discussions/341
  (fset 'forge--list-notifications-all
	    (symbol-function 'forge--list-notifications-unread))
  :bind
  ("C-c f l" . forge-edit-topic-labels)
  ("C-c f w" . forge-copy-url-at-point-as-kill)
  ("C-c f b" . forge-browse-pullreq))

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
  :after eww
  :preface
  (defun md4rd-open-post (url &rest _args)
    (md4rd--fetch-comments (concat (string-replace "www" "old" url) ".json")))
  :init
  (push (cons
         (rx line-start "http" (zero-or-one "s") "://www.reddit.com/" (one-or-more anychar))
         #'md4rd-open-post)
	    browse-url-handlers)
  :custom
  (md4rd-subs-active '(emacs clojure neovim))
  :config
  (add-hook 'md4rd-mode-hook 'md4rd-indent-all-the-lines))

(use-package pdf-tools
  :init
  (defun apt-abort-if-file-too-large-unless-pdf
      (fn size op-type filename &optional offer-raw)
    (unless (string-match-p "\\.pdf\\'" filename)
      (funcall fn size op-type filename offer-raw)))
  (defun apt-match-system-appearance
      ()
    (when (and (eql 'dark ns-system-appearance)
               (not (pdf-view-midnight-minor-mode)))
      (pdf-view-midnight-minor-mode)))
  (advice-add #'abort-if-file-too-large :around  #'apt-abort-if-file-too-large-unless-pdf)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; assumes (pdf-tools-install) worked once before
  (pdf-tools-install-noverify)
  (setq pdf-view-midnight-colors '("#ffffff" . "#000000"))
  (add-hook 'pdf-view-mode-hook 'apt-match-system-appearance))

(use-package denote
  :init
  (defun apt-commit-denote
      ()
    (interactive)
    (when (and buffer-file-name
               (string-prefix-p (expand-file-name "~/dropbox/denote") buffer-file-name))
      ;; as `start-process` is async, the first one might end before the second call.
      ;; but this does not matter much: in the worst case, new files will end up commited
      ;; anyway in the next save.
      (start-process "apt-git-add" nil "git" "add" ".")
      (start-process "apt-git-commit" nil "git" "commit" "-am" "Update")))
  (defun apt-denote-project
      ()
    (interactive)
    (project-switch-project "~/dropbox/denote"))
  (defun apt-open-journal
      ()
    (interactive)
    (find-file "/Users/andre.peric/dropbox/denote/20231110T141240--journal__nu.org"))
  (add-hook 'after-save-hook #'apt-commit-denote)
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
  :custom
  (denote-directory (expand-file-name "~/dropbox/denote"))
  (denote-known-keywords '("emacs" "tech"))
  (denote-dired-directories (list denote-directory))
  :bind
  ("C-S-d" . apt-denote-project)
  ("C-S-j" . apt-open-journal))

(use-package browse-url
  ;; enable when using xwidgets
  :disabled t
  :custom
  (browse-url-browser-function 'xwidget-webkit-browse-url)
  (browse-url-secondary-browser-function 'browse-url-default-browser))

(use-package shr
  :custom
  (shr-use-xwidgets-for-media t)
  (shr-max-width 65)
  (shr-inhibit-images t)
  (shr-use-colors nil)
  (shr-max-image-proportion 0.7)
  (shr-image-animate nil))

(use-package shr-heading
  :commands
  shr-heading-setup-imenu
  shr-heading-next
  shr-heading-previous)

(use-package eww
  :defer t
  :preface
  (defvar apt-reddit-url-regex
    (rx line-start "http" (zero-or-one "s") "://www.reddit.com/" (one-or-more anychar)))
  (defvar apt-stack-overflow-url-regex
    (rx line-start "http" (zero-or-one "s") "://stackoverflow.com/questions" (one-or-more anychar)))
  (defun apt-sx-open-link
      ()
    (interactive)
    (sx-open-link (eww-current-url)))
  (defun apt-exec-cmd-if-url-matches ()
    (cond ((string-match-p apt-reddit-url-regex (or (eww-current-url) ""))
           (message "Opening md4rd...")
           (md4rd-open-post (eww-current-url)))
          ((string-match-p apt-stack-overflow-url-regex (or (eww-current-url) ""))
           (message "Opening sx...")
           (apt-sx-open-link))))
  :custom
  (eww-search-prefix "https://www.google.com/search?q=")
  :bind
  (:map eww-mode-map
        ("x" . apt-sx-open-link)
        ;; ("P" . pocket-reader-eww-add-page)
        ("{" . backward-paragraph)
        ("}" . forward-paragraph)
        ("C-c C-p" . shr-heading-previous)
        ("C-c C-n" . shr-heading-next))
  :hook ((eww-mode . shr-heading-setup-imenu)
         (eww-after-render . apt-exec-cmd-if-url-matches)))

(use-package gif-screencast
  ;; on first run, might need to reset permissions:
  ;; https://apple.stackexchange.com/questions/374158/why-is-screencapture-taking-the-screenshot-of-the-desktop-image-and-not-the-wind
  :custom
  (gif-screencast-args '("-x")) ;; To shut up the shutter sound of `screencapture' (see `gif-screencast-command').
  (gif-screencast-cropping-program "mogrify") ;; Optional: Used to crop the capture to the Emacs frame.
  (gif-screencast-capture-format "ppm")
  :bind
  (("<f9>" . (lambda ()
	           (interactive)
	           (keycast-mode)
	           (gif-screencast-start-or-stop)))))

(use-package xwidget
  :bind
  (:map xwidget-webkit-mode-map
	    ("l" . xwidget-webkit-back)
	    ("r" . xwidget-webkit-forward)
	    ("o" . xwidget-webkit-browse-url)
	    ("g" . xwidget-webkit-reload)))

(use-package vc
  :bind
  ("C-x v F" . vc-pull))

(use-package link-hint
  :ensure t
  :bind
  ("C-c o" . link-hint-open-link)
  ("C-c o" . link-hint-copy-link))

(use-package sx
  :defer t)

(use-package nov
  :custom (nov-text-width 80)
  :mode ("\\.epub\\'" . nov-mode))

(use-package engine-mode
  :ensure t
  :custom
  (engine/browser-function 'eww-browse-url)
  :config
  (engine-mode t)
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "h")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"))

(use-package grip-mode)

(use-package sly
  :defer t
  :custom
  (inferior-lisp-program "clisp")
  :config
  (with-eval-after-load 'vilpy
    (add-to-list 'vilpy--handlers-alist
                 '(:sly . ((:decider-fn . (lambda () (bound-and-true-p sly-mode)))
                           (:eval-last-sexp . sly-eval-last-expression)
                           (:eval-defun . sly-eval-defun)
                           (:eval-region . sly-eval-region)
                           (:eval-buffer . sly-eval-buffer)
                           (:describe-symbol . sly-describe-symbol))))))

(use-package vc-backup
  :disabled t
  ;; usage: go to a backup file in the `backup-directory-alist`
  ;; and use `vc-print-log` (C-x v l)
  )

(use-package cape
  :bind (("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)))

(use-package olivetti
  :after org
  :hook ((org-mode . olivetti-mode)
         (markdown-mode . olivetti-mode)))

(use-package org
  :defer t
  :config
  ;; 'This variable needs to be set before org.el is loaded.'
  (add-to-list 'org-export-backends 'md)
  (add-to-list 'org-babel-load-languages '(dot . t))
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  :custom
  (org-directory "~/Dropbox/denote")
  (org-confirm-babel-evaluate nil)
  (org-special-ctrl-a/e t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")
  ;; See https://github.com/minad/org-modern/issues/15
  (org-modern-hide-stars nil)
  (org-hide-leading-stars t)
  (org-todo-keywords '((sequence "TODO" "DOING" "DONE"))))

(use-package org-modern
  :after org
  :custom
  (org-modern-todo-faces '(("DOING"
                            :background "blue"
                            :foreground "white")))
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  :config
  (defun apt-org-modern-spacing ()
    (setq-local line-spacing
                (if org-modern-mode
                    0.1 0.0)))
  :hook (org-modern-mode-hook . apt-org-modern-spacing))

(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))


(use-package jinx
  :config
  (setq jinx-languages "en_US pt_BR"))

(use-package treesit
  :preface
  (defun apt-install-ts-grammars ()
    (interactive)
    (dolist (grammar '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                       ;; For a more complete list, see
                       ;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
                       (yaml "https://github.com/ikatyang/tree-sitter-yaml")
                       (json "https://github.com/tree-sitter/tree-sitter-json")
                       (go "https://github.com/tree-sitter/tree-sitter-go")
                       (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  (dolist (mapping '(;; (yaml-mode . yaml-ts-mode)
                     (bash-mode . bash-ts-mode)
                     (js-mode . js-ts-mode)
                     (json-mode . json-ts-mode)
                     (clojure-mode . clojure-ts-mode)
                     (go-mode . go-ts-mode)
                     (dockerfile-mode . dockerfile-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (apt-install-ts-grammars))

(use-package yaml-pro
  :disabled t
  :hook (yaml-ts-mode . yaml-pro-ts-mode))

(use-package combobulate
  ;; cd ~/dev/peric && git clone https://github.com/mickeynp/combobulate.git 
  :preface
  (setq combobulate-key-prefix "M-g t")
  :hook ((js-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode))
  :load-path ("~/dev/peric/combobulate"))

(use-package go-ts-mode
  ;; for using `godoc-at-point':
  ;; go install github.com/zmb3/gogetdoc@latest
  :custom
  (go-ts-mode-indent-offset 4)
  (godoc-at-point-function 'godoc-gogetdoc))

(use-package eat
  ;; might require `(eat-compile-terminfo)` for handling keypresses correctly
  :config
  (add-to-list 'eat-semi-char-non-bound-keys [?\e ?o])
  (eat-update-semi-char-mode-map)
  ;; TO-DO: confirm if this is required
  ;; (eat-reload)
  )

(use-package surround
  :disabled t
  :ensure t
  :bind-keymap ("C-c s" . surround-keymap))

(use-package meow
  :ensure t
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '("<backtab>" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '(";" . meow-comment)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("F" . meow-find-expand)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("J" . delete-indentation)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("M" . set-mark-command)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-replace-pop)
     '("s" . meow-kill)
     '("S" . subword-mode)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("\\" . repeat)
     '("%" . meow-query-replace)
     '("&" . meow-query-replace-regexp)
     '("\\" . repeat)
     '("<escape>" . ignore)
     '("(" . meow-start-kmacro)
     '(")" . meow-end-kmacro)))
  :config
  (meow-setup)
  (setq meow-use-clipboard t)
  (meow-global-mode 1))

(use-package ellama
  :custom
  (llm-warn-on-nonfree nil)
  (ellama-auto-scroll t))

;; nu proj clone nu-llm.el
(use-package nu-llm
  :after ellama
  :load-path (lambda () (expand-file-name "nu-llm.el" (getenv "NU_HOME")))
  :config
  (add-to-list 'ellama-providers (cons "Nu OpenAI" (nu-llm-make-openai)))
  (when (eq 'work apt-profile)
    (setq ellama-provider (alist-get "Nu OpenAI" ellama-providers nil nil #'string=))))

(use-package dockerfile-ts-mode
  :config
  (setq auto-mode-alist
        (append
         '(("/Dockerfile\\.multistage\\'" . dockerfile-ts-mode))
         auto-mode-alist)))

(use-package magit
  :config
  (with-eval-after-load 'magit-status-mode
    (define-key magit-status-mode-map (kbd "K") 'magit-discard)))

(use-package org-download
  :config
  (setq org-download-image-dir "/Users/andre.peric/dropbox/denote/images"
        org-download-screenshot-method "screencapture"))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define vilpy-mode-map "jk" 'vilpy-special))
