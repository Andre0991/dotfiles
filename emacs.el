
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
				  yaml-mode
				  markdown-mode
				  evil
				  avy
				  inf-clojure
				  embark
				  embark-consult
				  eglot
				  browse-at-remote
				  corfu))


;;; Global keybindings
(let ((map global-map))
  (define-key map (kbd "M-o") #'other-window))
(when (string= (system-name) "Andres-MBP-2.lan") ; work macbook
  ;; translate super to control
  (setq ns-command-modifier 'control))


;;; Emacs
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
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
      tab-always-indent 'complete
      compilation-scroll-output 't)


;;; Themes
(modus-themes-load-themes)
(modus-themes-load-vivendi)


;;; Face
(set-face-attribute 'default nil :height 150)


;;; Evil
(require 'evil)
;; (evil-mode 1)
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
(evil-define-key 'normal 'global (kbd "<leader>cl") 'consult-line)
(evil-define-key 'normal 'global (kbd "<leader>ci") 'consult-imenu)
(evil-define-key 'normal 'global (kbd "<leader>cy") 'consult-yank)
(evil-define-key 'normal 'global (kbd "<leader>cg") 'consult-global-mark)



;;; Embark
(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)))
(define-key global-map (kbd "C-;") #'embark-act)


;;; Orderless
(require 'orderless)
(setq completion-styles '(orderless))


;;; Vilpy
(let ((vilpy-path "~/dev/peric/vilpy/"))
  (add-to-list 'load-path vilpy-path)
  (require 'vilpy))
(add-hook 'emacs-lisp-mode-hook (lambda () (vilpy-mode 1)))
(add-hook 'clojure-mode-hook (lambda () (vilpy-mode 1)))
(evil-define-key 'insert 'vilpy-mode-map (kbd "C-k") 'vilpy-kill)
(evil-define-key 'insert 'vilpy-mode-map (kbd "C-d") 'vilpy-delete)
(evil-define-key 'insert 'vilpy-mode-map (kbd "C-y") 'vilpy-yank)


;;; Env variables
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;;; Clojure
(setq clojure-align-forms-automatically t)


;;; Eglot
(setq eglot-confirm-server-initiated-edits nil)
(setq eglot-connect-timeout 300)
(with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(clojure-mode . ("clojure-lsp"))))
(define-key eglot-mode-map (kbd "C-c l a") #'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-c l l") #'eglot)
(define-key eglot-mode-map (kbd "C-c l q") #'eglot-shutdown)
(define-key eglot-mode-map (kbd "C-c l r") #'eglot-rename)
(define-key eglot-mode-map (kbd "C-c l u") #'xref-find-references)
(evil-define-key 'normal 'global (kbd "<leader>la") 'eglot-code-actions)
(evil-define-key 'normal 'global (kbd "<leader>ll") 'eglot)
(evil-define-key 'normal 'global (kbd "<leader>lq") 'eglot-shutdown)
(evil-define-key 'normal 'global (kbd "<leader>lr") 'eglot-rename)
(evil-define-key 'normal 'global (kbd "<leader>lu") 'xref-find-references)


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


;;; project
;; TODO: If file does not exist, ask if want to create it.
(defun apt-project-switch-between-test-and-implementation ()
    (interactive)
    (cond ((derived-mode-p 'clojure-mode)
	   (let* ((src-dir "/src/")
		  (test-dir "/test/")
		  (test-suffix "_test")
		  (is-test-p (cl-search test-dir buffer-file-name)))
	     (if is-test-p
	       (find-file (format "%s.%s"
                                  (thread-last buffer-file-name
                                               file-name-sans-extension
                                               (string-replace test-dir src-dir)
                                               (replace-regexp-in-string "_test$" ""))
				  (file-name-extension buffer-file-name)))
               (find-file (format "%s%s.%s"
				  (string-replace src-dir test-dir (file-name-sans-extension buffer-file-name))
				  test-suffix
				  (file-name-extension buffer-file-name))))))
	  ((derived-mode-p 'emacs-lisp-mode)
	   (let* ((test-suffix "-test")
		  (is-test-p (cl-search test-suffix buffer-file-name)))
	     (if is-test-p
	       (find-file (format "%s.%s"
                                  (thread-last buffer-file-name
                                               file-name-sans-extension
                                               (replace-regexp-in-string "-test$" ""))
				  (file-name-extension buffer-file-name)))
               (find-file (format "%s%s.%s"
				  (file-name-sans-extension buffer-file-name)
				  test-suffix
				  (file-name-extension buffer-file-name))))))))

(evil-define-key 'normal 'global (kbd "<leader>pf") 'project-find-file)
(evil-define-key 'normal 'global (kbd "<leader>pp") 'project-switch-project)
(evil-define-key 'normal 'global (kbd "<leader>pc") 'project-compile)
(evil-define-key 'normal 'global (kbd "<leader>p!") 'project-shell-command)
(evil-define-key 'normal 'global (kbd "<leader>pr") 'consult-ripgrep)
(evil-define-key 'normal 'global (kbd "<leader>p&") 'project-async-shell-command)
(evil-define-key 'normal 'global (kbd "<leader>pF") 'project-or-external-find-file)
(evil-define-key 'normal 'global (kbd "<leader>pG") 'project-or-external-find-regexp)
(evil-define-key 'normal 'global (kbd "<leader>pb") 'project-switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>pd") 'project-dired)
(evil-define-key 'normal 'global (kbd "<leader>pe") 'project-eshell)
(evil-define-key 'normal 'global (kbd "<leader>pg") 'project-find-regexp)
(evil-define-key 'normal 'global (kbd "<leader>pk") 'project-kill-buffers)
(evil-define-key 'normal 'global (kbd "<leader>p%") 'project-query-replace-regexp)
(evil-define-key 'normal 'global (kbd "<leader>pv") 'project-vc-dir)
(evil-define-key 'normal 'global (kbd "<leader>px") 'project-execute-extended-command)
;; my extras
(evil-define-key 'normal 'global (kbd "<leader>pt") 'apt-project-switch-between-test-and-implementation)


;;; Misc leader keybindings
(evil-define-key 'normal 'global (kbd "<leader><SPC>") 'execute-extended-command)
;; buffer
(evil-define-key 'normal 'global (kbd "<leader>bb") 'consult-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bo") 'consult-buffer-other-window)
(evil-define-key 'normal 'global (kbd "<leader>bs") 'switch-to-buffer)
;; file
(evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)


;;; Flymake
(evil-define-key 'normal 'global (kbd "<leader>cf") 'consult-flymake)


;;; Magit
(evil-define-key 'normal 'global (kbd "<leader>gg") 'magit)


;; Window
(evil-define-key 'normal 'global (kbd "<leader>ww") 'other-window)
(evil-define-key 'normal 'global (kbd "<leader>w/") 'split-window-right)
(evil-define-key 'normal 'global (kbd "<leader>w-") 'split-window-vertically)
(evil-define-key 'normal 'global (kbd "<leader>wd") 'delete-window)
(evil-define-key 'normal 'global (kbd "<leader>wm") 'maximize-window)
(evil-define-key 'normal 'global (kbd "<leader>wk") 'windmove-up)
(evil-define-key 'normal 'global (kbd "<leader>wj") 'windmove-down)
(evil-define-key 'normal 'global (kbd "<leader>wh") 'windmove-left)
(evil-define-key 'normal 'global (kbd "<leader>wl") 'windmove-right)
(evil-define-key 'normal 'global (kbd "<leader>wD") 'delete-other-windows)


;; Outline
(evil-define-key 'normal 'global (kbd "<leader>os") 'outline-show-subtree)
(evil-define-key 'normal 'global (kbd "<leader>oh") 'outline-hide-subtree)
(evil-define-key 'normal 'global (kbd "<leader>oc") 'outline-cycle)
(evil-define-key 'normal 'global (kbd "<leader>oc") 'outline-hide)
(evil-define-key 'normal 'global (kbd "<leader>on") 'outline-next-heading)
(evil-define-key 'normal 'global (kbd "<leader>op") 'outline-previous-heading)

;; emacs
(evil-define-key 'normal 'global (kbd "<leader>hv") 'describe-variable)
(evil-define-key 'normal 'global (kbd "<leader>hf") 'describe-function)


;;; Helpers
(defun check-next-def ()
  (push-mark nil t)
  (when (re-search-forward
         (concat "(def\\(?:un\\|macro\\|subst\\|var\\|const\\) "
                 "\\(\\(?:\\sw\\|\\s_\\)+\\)")
         nil 'move)
    (save-excursion
      (let ((name (match-string 1)))
        (goto-char (point-min))
        (unless (re-search-forward (concat "\\_<" name "\\_>") nil t 2)
          name)))))

(defun find-unused-def ()
  (interactive)
  (let (name)
    (while (and (not (eobp))
                (not (setq name (check-next-def)))))
    (message "Found! %s" name)))


;;; TODO
;; select candiates in other buffer (embark?)
;; fix exec-path-from-shell-initialize performance
;; set localleader and use it for some modes (evil-set-leader STATE KEY [LOCALLEADER]) (see https://evil.readthedocs.io/en/latest/keymaps.html#leader-keys)
;; leader key does not work on visual selectio n
;; selection + * won't work
;; iedit or alternative
;; add yaml mode
;; add outlijne binding 
