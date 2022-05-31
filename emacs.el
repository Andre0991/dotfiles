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
				  package-lint
				  olivetti
				  orderless
				  vertico
				  wgrep
				  which-key
				  yaml-mode))

(when (display-graphic-p)
  (tool-bar-mode -1)
  (require 'auto-dark)
  (setq auto-dark--light-theme 'modus-operandi)
  (setq auto-dark--dark-theme 'modus-vivendi))


;;; Global keybindings
(define-key global-map (kbd "M-o") #'other-window)
(define-key global-map (kbd "M-O") #'other-frame)
(define-key global-map (kbd "M-,") 'pop-tag-mark)
(define-key global-map (kbd "C-c f d") 'delete-file)
(define-key global-map (kbd "M-Z") 'zap-up-to-char)
(when (string= system-type 'darwin)
  ;; translate super to control
  (setq ns-command-modifier 'control))

;;; Load paths
(dolist (path '("~/dotfiles/elisp/"
		"~/Dropbox/nu/emacs-lisp"))
  (when (file-directory-p path)
    (add-to-list 'load-path path)))

;;; Emacs
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
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
      ;; TAB cycle if there are only few candidates
      completion-cycle-threshold 3
      ;; Enable indentation+completion using the TAB key.
      ;; Completion is often bound to M-TAB.
      tab-always-indent 'complete
      compilation-scroll-output 't
      sentence-end-double-space nil
      auth-sources '("~/.authinfo")
      isearch-lazy-count t
      async-shell-command-buffer 'new-buffer
      ;; when there are two dired windows, automatically considers
      ;; the other as a target (for copying a file, for example)
      dired-dwim-target t)
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

;;; Comint
(setq comint-scroll-to-bottom-on-output 'others
      comint-input-ignoredups t)

;;; Windmove
(windmove-default-keybindings)

;;; Flymake
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;;; Eldoc
;; This displays full docs for clojure functions.
;; See https://github.com/joaotavora/eglot/discussions/894
(setq eldoc-documentation-strategy 'eldoc-documentation-compose
      eldoc-echo-area-use-multiline-p 5)

;; Webjump
(setq webjump-sites
      '(("Tekton Dashboard" . "https://dashboard.cicd.nubank.world/")
        ("The Drunkard's Walk"   . "https://meet.google.com/jgf-bogh-oop")
	("Eagles eyes"   . "https://meet.google.com/xgc-wmqr-kvn")
	("Zoom"   . "https://nubank.zoom.us/j/6441464215")
	("Jira"   . "https://nubank.atlassian.net/jira/software/c/projects/CPL/boards/277")))

;;; Pulse
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))
(dolist (command '(scroll-up-command
		   scroll-down-command
                   recenter-top-bottom
		   other-window
		   other-frame))
  (advice-add command :after #'pulse-line))

;;; Electric pair mode
(electric-pair-mode)
(defun apt-inhibit-electric-pair-mode (_char)
  ;; do not use electrict pair mode in minifuffer, as
  ;; it is common to use a single parens for searching
  (minibufferp))
(setq electric-pair-inhibit-predicate #'apt-inhibit-electric-pair-mode)

;;; Face
(set-face-attribute 'default nil :height 190 :family "DejaVu Sans Mono")
(set-face-attribute 'variable-pitch nil :family "Helvetica" :height 210)

;;; Themes
(setq modus-themes-scale-headings t)

;;; Writing
(dolist (hook '(markdown-mode-hook
		org-mode-hook))
  ;; experimental: is it too intrusive to be automatic?
  ;; (add-hook hook 'olivetti-mode)
  (add-hook hook 'variable-pitch-mode))

;;; Markdown mode
(setq markdown-fontify-code-blocks-natively t)
(with-eval-after-load 'markdown-mode
  (dolist (face '(markdown-inline-code-face
		  markdown-code-face
		  markdown-table-face))
    (set-face-attribute face nil :height 190 :family "DejaVu Sans Mono")))

;;; Winner mode
(add-hook 'after-init-hook #'winner-mode)

;;; Which-key
;; Manual Activation 
;; Allow C-h to trigger which-key before it is done automatically
(setq which-key-show-early-on-C-h t
      ;; make sure which-key doesn't show normally but refreshes quickly after it is
      ;; triggered.
      which-key-idle-delay 10000
      which-key-idle-secondary-delay 0.05)
(which-key-mode)

;;; Marginalia
(defun marginalia-use-builtin ()
  (mapc
   (lambda (x)
     (setcdr x (cons 'builtin (remq 'builtin (cdr x)))))
   marginalia-annotator-registry))
(marginalia-mode)
(marginalia-use-builtin)

;;; Vertico
(vertico-mode)
(with-eval-after-load 'vertico
  (setq vertico-cycle nil)
  (define-key vertico-map "\M-q" #'vertico-quick-exit))

;;; Corfu
(add-hook 'prog-mode-hook 'corfu-mode)
(add-hook 'shell-mode-hook 'corfu-mode)
(with-eval-after-load 'corfu
  (setq corfu-auto nil)
  (define-key corfu-map (kbd "SPC") #'corfu-insert-separator))

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
(setq completion-styles '(orderless))

;; Vilpy
(let ((vilpy-path "~/dev/peric/vilpy/"))
  (add-to-list 'load-path vilpy-path)
  (require 'vilpy))
(add-hook 'emacs-lisp-mode-hook (lambda () (vilpy-mode 1)))
(add-hook 'clojure-mode-hook (lambda () (vilpy-mode 1)))

;;; Clojure
(setq clojure-align-forms-automatically t)

;;; Eglot
(setq eglot-confirm-server-initiated-edits nil)
(setq eglot-connect-timeout 300)
(add-hook 'clojure-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c l a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l l") #'eglot)
  (define-key eglot-mode-map (kbd "C-c l q") #'eglot-shutdown)
  (define-key eglot-mode-map (kbd "C-c l r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l u") #'xref-find-references))

;;; inf-clojure
;; from `elisp-path` 
(require 'apt-inf-clojure nil 'noerror)

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
;; (with-eval-after-load 'consult
;;   (add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep" ?r))∑)

;; smerge
(require 'apt-smerge-extras nil 'noerror)

;;; tab bar
(require 'apt-tab-bar-extras nil 'noerror)

;;; Helpers
(require 'apt-helpers nil 'noerror)

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
