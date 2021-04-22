
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
				  embark-consult))



;;; Emacs
(global-display-line-numbers-mode)
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
      initial-major-mode 'fundamental-mode)


;;; Themes
(modus-themes-load-themes)
(modus-themes-load-vivendi)


;;; Face
(set-face-attribute 'default nil :height 150)


;;; Vertico
(vertico-mode)


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
  (define-key embark-meta-map (kbd "C-o") #'embark-keymap-help))


;;; Orderless
(require 'orderless)
(setq completion-styles '(orderless))


;;; Viper
(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)


;;; Evil
(require 'evil)
(evil-mode 1)
(evil-set-leader 'normal (kbd "SPC"))
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
(evil-define-key 'normal 'global (kbd "<leader>p&") 'project-async-shell-command)
;; magit
(evil-define-key 'normal 'global (kbd "<leader>gg") 'magit)
;; consult
(evil-define-key 'normal 'global (kbd "<leader>cl") 'consult-line)
;; window
(evil-define-key 'normal 'global (kbd "<leader>ww") 'other-window)
(evil-define-key 'normal 'global (kbd "<leader>w/") 'split-window-right)
(evil-define-key 'normal 'global (kbd "<leader>wd") 'delete-window)
(evil-define-key 'normal 'global (kbd "<leader>wm") 'maximize-window)


;;; Vilpy
(let ((vilpy-path "~/dev/peric/vilpy/"))
  (add-to-list 'load-path vilpy-path)
  (require 'vilpy))
(add-hook 'emacs-lisp-mode-hook (lambda () (vilpy-mode 1)))
(add-hook 'clojure-mode-hook (lambda () (vilpy-mode 1)))


;;; Env variables
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; TODO
;; god-like mode (see https://www.reddit.com/r/emacs/comments/apvpeu/status_of_godmode/)
;; select candiates in other buffer (embark?)
;; inf-clojure
;; cider
;; window undo
;; line numbers not in eww, help buffers and other modes
;; fix exec-path-from-shell-initialize performance
;; set localleader and use it for some modes (evil-set-leader STATE KEY [LOCALLEADER]) (see https://evil.readthedocs.io/en/latest/keymaps.html#leader-keys)

