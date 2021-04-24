
;;; Packages

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; Install missing packages with (package-install-selected-packages)
(setq package-selected-packages '(modus-themes
				  vertico
				  magit
				  exec-path-from-shell
				  consult
				  orderless
				  clojure-mode
				  markdown-mode
				  evil
				  avy
				  inf-clojure
				  embark
				  embark-consult
				  eglot
				  browse-at-remote
				  corfu
				  marginalia))



;;; Emacs
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))
(setq vc-follow-symlinks t
      recentf-mode t
      enable-recursive-minibuffers t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      initial-major-mode 'fundamental-mode
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/")))
      ;; TAB cycle if there are only few candidates
      completion-cycle-threshold 3
      ;; Enable indentation+completion using the TAB key.
      ;; Completion is often bound to M-TAB.
      tab-always-indent 'complete)


;;; Themes
(modus-themes-load-themes)
(modus-themes-load-vivendi)


;;; Face
(set-face-attribute 'default nil :height 150)


;;; Evil
(require 'evil)
(evil-mode 1)
(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'normal (kbd ",") t) 	; localleader


;;; Winner mode
(add-hook 'after-init-hook #'winner-mode)
(evil-define-key 'normal 'global (kbd "<leader>wu") 'winner-undo)
(evil-define-key 'normal 'global (kbd "<leader>wr") 'winner-redo)


;;; Vertico
(vertico-mode)


;;; Corfu
(add-hook 'prog-mode-hook 'corfu-mode)


;;; Consult
(setq consult-project-root-function
      (lambda ()
        (when-let (project (project-current))
          (project-root project)))
      xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(let ((map global-map))
  (define-key map (kbd "C-x b") #'consult-buffer))


;;; Embark
(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)))

(with-eval-after-load 'embark
  (define-key global-map (kbd "C-o") #'embark-act))


;;; Orderless
(require 'orderless)
(setq completion-styles '(orderless))


;;; Vilpy
(let ((vilpy-path "~/dev/peric/vilpy/"))
  (add-to-list 'load-path vilpy-path)
  (require 'vilpy))
(add-hook 'emacs-lisp-mode-hook (lambda () (vilpy-mode 1)))
(add-hook 'clojure-mode-hook (lambda () (vilpy-mode 1)))


;;; Env variables
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;;; eglot
(setq eglot-confirm-server-initiated-edits nil)
(setq eglot-connect-timeout 300)
(with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(clojure-mode . ("clojure-lsp"))))
(evil-define-key 'normal 'global (kbd "<leader>la") 'eglot-code-actions)
(evil-define-key 'normal 'global (kbd "<leader>ll") 'eglot)
(evil-define-key 'normal 'global (kbd "<leader>lq") 'eglot-shutdown)
(evil-define-key 'normal 'global (kbd "<leader>lr") 'eglot-rename)
(evil-define-key 'normal 'global (kbd "<leader>ld") 'xref-find-definitions)


;;; inf-clojure
(evil-define-key 'normal 'inf-clojure-minor-mode-map (kbd "<localleader>eb") 'inf-clojure-eval-buffer)
(evil-define-key 'normal 'inf-clojure-minor-mode-map (kbd "<localleader>rn") 'inf-clojure-set-ns)
(evil-define-key 'normal 'inf-clojure-minor-mode-map (kbd "<localleader>rr") 'inf-clojure-switch-to-repl)
(let ((elisp-path "~/dotfiles/elisp/"))
  (when (file-directory-p elisp-path)
    (add-to-list 'load-path elisp-path)
    (require 'apt-inf-clojure)))


;;; eww
(setq eww-search-prefix "https://www.google.com/search?q=")


;;; browse-at-remote
(evil-define-key 'normal 'global (kbd "<leader>go") 'browse-at-remote)


;;; Leader keybindings
(evil-define-key 'normal 'global (kbd "<leader><SPC>") 'execute-extended-command)
;; buffer
(evil-define-key 'normal 'global (kbd "<leader>bb") 'consult-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bs") 'switch-to-buffer)
;; file
(evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
;; project
;; TODO: Continue this. See the `project-prefix-map` keymap
(evil-define-key 'normal 'global (kbd "<leader>pf") 'project-find-file)
(evil-define-key 'normal 'global (kbd "<leader>pp") 'project-switch-project)
(evil-define-key 'normal 'global (kbd "<leader>pc") 'project-compile)
(evil-define-key 'normal 'global (kbd "<leader>p!") 'project-shell-command)
(evil-define-key 'normal 'global (kbd "<leader>pr") 'consult-ripgrep)
(evil-define-key 'normal 'global (kbd "<leader>p&") 'project-async-shell-command)
;; flymake
(evil-define-key 'normal 'global (kbd "<leader>cf") 'consult-flymake)
;; magit
(evil-define-key 'normal 'global (kbd "<leader>gg") 'magit)
;; consult
(evil-define-key 'normal 'global (kbd "<leader>cl") 'consult-line)
(evil-define-key 'normal 'global (kbd "<leader>ci") 'consult-imenu)
;; window
(evil-define-key 'normal 'global (kbd "<leader>ww") 'other-window)
(evil-define-key 'normal 'global (kbd "<leader>w/") 'split-window-right)
(evil-define-key 'normal 'global (kbd "<leader>wd") 'delete-window)
(evil-define-key 'normal 'global (kbd "<leader>wm") 'maximize-window)
(evil-define-key 'normal 'global (kbd "<leader>wh") 'windmove-left)
(evil-define-key 'normal 'global (kbd "<leader>wl") 'windmove-right)
(evil-define-key 'normal 'global (kbd "<leader>wl") 'windmove-right)
;; emacs
(evil-define-key 'normal 'global (kbd "<leader>hv") 'describe-variable)
(evil-define-key 'normal 'global (kbd "<leader>hf") 'describe-function)


;;; TODO
;; god-like mode (see https://www.reddit.com/r/emacs/comments/apvpeu/status_of_godmode/)
;; select candiates in other buffer (embark?)
;; fix exec-path-from-shell-initialize performance
;; set localleader and use it for some modes (evil-set-leader STATE KEY [LOCALLEADER]) (see https://evil.readthedocs.io/en/latest/keymaps.html#leader-keys)
;; leader key does not work on visual selection
;; selection + * won't work
;; iedit or alternative
;; switch between test and implementation


;; TODO: Finish test
(defun apt-switch-between-test-and-implementation ()
  (interactive)
  (cond ((derived-mode-p 'clojure-mode)
	 (find-file (format "%s_test.%s"
			  (string-replace "/src/" "/test/"
					  (file-name-sans-extension buffer-file-name))
			  (file-name-extension buffer-file-name))))))
