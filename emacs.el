
;;; Packages

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; Install missing packages:
;; (package-install-selected-packages)

;; Remove unused packages:
;; (package-autoremove)
(setq package-selected-packages '(modus-themes
				  vertico
				  magit
				  consult
				  orderless
				  wgrep
				  clojure-mode
				  yaml-mode
				  markdown-mode
				  avy
				  inf-clojure
				  embark
				  embark-consult
				  eglot
				  browse-at-remote
				  corfu
				  which-key
				  olivetti
				  ox-slack
				  flymake-shellcheck
				  forge
				  marginalia))


;;; Global keybindings
(let ((map global-map))
  (define-key map (kbd "M-o") #'other-window)
  (define-key map (kbd "M-,") 'pop-tag-mark)
  (define-key map (kbd "C-c f d") 'delete-file)
  (define-key map (kbd "M-Z") 'zap-up-to-char))
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
(when (display-graphic-p)
  (tool-bar-mode -1))
(defalias 'yes-or-no-p 'y-or-n-p)
;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))
(setq vc-follow-symlinks t
      recentf-mode t
      enable-recursive-minibuffers t
      initial-scratch-message nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      initial-major-mode 'fundamental-mode
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/")))
      ;; TAB cycle if there are only few candidates
      completion-cycle-threshold 3
      ;; Enable indentation+completion using the TAB key.
      ;; Completion is often bound to M-TAB.
      tab-always-indent 'complete
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

;; TODO: Create only if it does not exist
;; (make-directory "~/.emacs.d/aux")
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t)))
(setq auto-save-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t)))
(setq backup-directory-alist
      '((".*" . "~/.emacs.d/aux/")))



;;; Windmove
(windmove-default-keybindings)


;;; Pulse
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command
		   scroll-down-command
                   recenter-top-bottom
		   other-window))
  (advice-add command :after #'pulse-line))


;;; Electric pair mode
(electric-pair-mode)
(defun apt-inhibit-electric-pair-mode (_char)
  ;; do not use electrict pair mode in minifuffer, as
  ;; it is common to use a single parens for searching
  (minibufferp))
(setq electric-pair-inhibit-predicate #'apt-inhibit-electric-pair-mode)


;;; Themes
(setq auto-dark-emacs/light-theme 'modus-operandi)
(setq auto-dark-emacs/dark-theme 'modus-vivendi)
(setq modus-themes-scale-headings t)
(when (display-graphic-p)
  (require 'auto-dark-emacs))


;;; Face
(set-face-attribute 'default nil :height 180 :family "DejaVu Sans Mono")
(set-face-attribute 'variable-pitch nil :family "Helvetica" :height 210)


;;; Writing
(dolist (hook '(markdown-mode-hook
		org-mode-hook))
  ;; experimental: is it too intrusive to be automatic?
  ;; (add-hook hook 'olivetti-mode)
  (add-hook hook 'variable-pitch-mode))


;;; Winner mode
(add-hook 'after-init-hook #'winner-mode)


;;; Which-key
;; Manual Activation 
;; Allow C-h to trigger which-key before it is done automatically
(setq which-key-show-early-on-C-h t)
;; make sure which-key doesn't show normally but refreshes quickly after it is
;; triggered.
(setq which-key-idle-delay 10000)
(which-key-mode)
(setq which-key-idle-secondary-delay 0.05)


;;; Marginalia
(marginalia-mode)


;;; Vertico
(vertico-mode)
(with-eval-after-load 'vertico
  (setq vertico-cycle nil))


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


;;; Lispy
;; (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))


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
(require 'apt-inf-clojure)
(with-eval-after-load 'inf-clojure
  (vilpy-define-key vilpy-mode-map "d" 'inf-clojure-set-ns))


;;; eww
(setq eww-search-prefix "https://www.google.com/search?q=")


;;; browse-at-remote
(global-set-key (kbd "C-c g o") 'browse-at-remote)


;;; project
(require 'apt-project-extras)
(define-key project-prefix-map (kbd "t") 'apt-project-switch-between-test-and-implementation)
;; (with-eval-after-load 'consult
;;   (add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep" ?r)))


;;; Helpers
(require 'apt-helpers)


;;; Nu
(require 'nu-andre)


;;; Org
(setq org-todo-keywords '((sequence "TODO(t)" "IN PROGRESS (p)" "WAITING(w)" "|" "DONE(d)"))
      org-agenda-files '("/Users/andreperictavares/Dropbox/nu/org/tasks/TODO.org")
      org-html-postamble nil)
(global-set-key (kbd "C-c L") 'org-store-link)
(setq org-publish-project-alist
      '(("mothership-proposal"
	 :base-directory "~/Dropbox/nu/org/tasks/mothership-proposal/"
	 :publishing-directory "~/Dropbox/nu/org/tasks/mothership-proposal/html_export/"
	 :publishing-function org-html-publish-to-html
	 :auto-sitemap t
	 :recursive t)))


;;; Shell
(add-hook 'sh-mode-hook 'flymake-shellcheck-load)


;;; Forge
(with-eval-after-load 'magit
  (require 'forge))


;;; ERC
(setq erc-server "irc.libera.chat")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((cider-shadow-cljs-default-options . "app")
     (cider-default-cljs-repl . shadow))))
