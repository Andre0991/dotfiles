
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
				  which-key))


;;; Global keybindings
(let ((map global-map))
  (define-key map (kbd "M-o") #'other-window)
  (define-key map (kbd "M-,") 'pop-tag-mark)
  (define-key map (kbd "C-c f d") 'delete-file))
(when (string= system-type 'darwin)
  ;; translate super to control
  (setq ns-command-modifier 'control))


;;; Load paths
(let ((elisp-path "~/dotfiles/elisp/"))
  (when (file-directory-p elisp-path)
    (add-to-list 'load-path elisp-path)))


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
      compilation-scroll-output 't)

;;; electric pair mode
(electric-pair-mode)
(defun apt-inhibit-electric-pair-mode (_char)
  (minibufferp))
(setq electric-pair-inhibit-predicate #'apt-inhibit-electric-pair-mode)


;;; Themes
(setq auto-dark-emacs/light-theme 'modus-operandi)
(setq auto-dark-emacs/dark-theme 'modus-vivendi)
(require 'auto-dark-emacs)


;;; Face
(set-face-attribute 'default nil :height 150)


;;; Winner mode
(add-hook 'after-init-hook #'winner-mode)


;;; which-key
;; Manual Activation 
;; Allow C-h to trigger which-key before it is done automatically
(setq which-key-show-early-on-C-h t)
;; make sure which-key doesn't show normally but refreshes quickly after it is
;; triggered.
(setq which-key-idle-delay 10000)
(setq which-key-idle-secondary-delay 0.05)
(which-key-mode)


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
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(clojure-mode . ("clojure-lsp")))
  (define-key eglot-mode-map (kbd "C-c l a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l l") #'eglot)
  (define-key eglot-mode-map (kbd "C-c l q") #'eglot-shutdown)
  (define-key eglot-mode-map (kbd "C-c l r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l u") #'xref-find-references))


;;; inf-clojure
;; from `elisp-path` 
(require 'apt-inf-clojure)
(with-eval-after-load 'inf-clojure
  (define-key inf-clojure-mode-map (kbd "C-c m t") #'apt-inf-clojure-run-test-at-point))


;;; eww
(setq eww-search-prefix "https://www.google.com/search?q=")


;;; browse-at-remote
(global-set-key (kbd "C-c g o") 'browse-at-remote)


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
(define-key project-prefix-map (kbd "t") 'apt-project-switch-between-test-and-implementation)


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
