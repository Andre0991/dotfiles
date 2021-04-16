;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "André Peric Tavares"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Source Code Pro" :size 15)
      doom-variable-pitch-font (font-spec :family "Helvetica" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 't)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; general keybindings

(map! "C-h" #'delete-backward-char)
(map! "C-;" #'iedit-mode)
(setq doom-localleader-key ",")

;; general settings

(setq confirm-kill-processes nil)
(setq doom-scratch-initial-major-mode 'markdown-mode)
(setq compilation-scroll-output 't)

;; util

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun raise-minor-mode (mode)
  "Make MODE the first on `minor-mode-map-alist'."
  (let ((x (assq mode minor-mode-map-alist)))
    (when x
      (setq minor-mode-map-alist
            (cons x (delq mode minor-mode-map-alist))))))

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

(defun apt-inf-clojure-init-repl ()
  "Sane options for using a repl."
  (inf-clojure-eval-string "(do (require '[clojure.main :as m])
                                (require '[clojure.pprint :as pp])
                                (clojure.main/repl :print pp/pprint
                                                   :init #(do
                                                           ;; The :init hook replicates what clojure.main uses as an initial binding set, so it’s good to repeat that.
                                                           ;; From https://insideclojure.org/2020/02/11/custom-repl/
                                                           (apply require m/repl-requires)
                                                           ;; Workaround for rebinding clojure.test/*test-out*.
                                                           ;; For details, see https://github.com/nrepl/nrepl/blob/a057874cd3bfc83e465cc163fbc1d4c00223e1b1/src/clojure/nrepl/middleware/interruptible_eval.clj#L93-L99\"
                                                           (require 'clojure.test)
                                                           (let [bindings (into (get-thread-bindings)
                                                                                {#'clojure.test/*test-out* *out*})]
                                                             (pop-thread-bindings)
                                                             (push-thread-bindings bindings)))))"))

(defun apt-inf-clojure-doc ()
  (interactive)
  (inf-clojure-eval-string (format "(clojure.repl/doc %s)"
                                   (lispy--current-function))))

(defun apt-inf-clojure-run-test-at-point ()
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (end-of-line)
    (inf-clojure-eval-string (format "(%s)"
                                     (symbol-at-point)))))

(defun apt-inf-clojure-connect ()
  (interactive)
  (inf-clojure-connect "localhost" "5555")
  (apt-inf-clojure-init-repl))

(defun apt-inf-clojure-source ()
  (interactive)
  (inf-clojure-eval-string (format "(clojure.repl/source %s)"
                                   (lispy--current-function))))

(defun apt-inf-clojure-refresh-all ()
  (interactive)
  (inf-clojure-eval-string "(do (require 'clojure.tools.namespace.repl)
                                (clojure.tools.namespace.repl/refresh-all))"))

(defun apt-inf-clojure-refresh ()
  (interactive)
  (inf-clojure-eval-string "(do (require 'clojure.tools.namespace.repl)
                                (clojure.tools.namespace.repl/refresh))"))

(defun apt-lispy-describe ()
  (interactive)
  (if (bound-and-true-p inf-clojure-minor-mode)
      (call-interactively 'apt-inf-clojure-doc)
    (call-interactively 'lispy-describe)))

(defun apt-inf-clojure-add-dependency ()
  (interactive)
  (let* ((dependency (read-string "Dependency: "))
         (version (read-string "Version (default LATEST): " nil nil "LATEST"))
         (clj-expr (format "(require 'cemerick.pomegranate)
 (require 'cemerick.pomegranate.aether)
 (cemerick.pomegranate/add-dependencies :coordinates '[[%s \"%s\"]]
                                        :repositories (merge cemerick.pomegranate.aether/maven-central
                                                             {\"clojars\" \"https://clojars.org/repo\"}
                                                             {\"nu-maven\" \"s3p://nu-maven/releases/\"}))"
                           dependency
                           version)))
    (message clj-expr)
    (inf-clojure-eval-string clj-expr)))

(defun apt-lispy-g ()
  (interactive)
  (cl-case (read-char-from-minibuffer "Actions:\n
n: Go to next error
p: Go to previous error
s: Show source code
w: Save buffer
\n")
    (?n (call-interactively 'flymake-goto-next-error))
    (?p (call-interactively 'flymake-goto-prev-error))
    (?s (call-interactively 'apt-inf-clojure-source))
    (?w (save-buffer))))


;; auth sources
(add-to-list 'auth-sources "~/.authinfo")

;; local packages

(let ((nudev-emacs-path "~/dev/nu/nudev/ides/emacs/"))
  (when (file-directory-p nudev-emacs-path)
    (add-to-list 'load-path nudev-emacs-path)
    (require 'nu)))

(load! "~/Dropbox/nu/emacs-lisp/nu-andre.el" nil t)

(use-package nucli
  :init (setq nucli-commands-per-section 28)
  :commands (nucli)
  :load-path ("~/dev/nu/nucli.el/src"))

;; packages
(use-package lispy
   :hook ((lisp-mode . lispy-mode)
          (emacs-lisp-mode . lispy-mode)
          (clojure-mode . lispy-mode))
   :load-path "~/dev/peric/lispy-lite"
   :config (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode))

(after! lispy
  (setq lispy-eval-display-style 'overlay)
  (defun andre/lispy-cider-pprint ()
    (interactive)
    (lispy-different)
    (cider-pprint-eval-last-sexp)
    (lispy-different))
  (defun andre/cider-clojuredocs ()
    (interactive)
    (save-excursion
      (forward-char)
      (let ((cider-prompt-for-symbol nil))
        (cider-clojuredocs))))
  ;; prevents `/` (`lispy-splice`) from being overriden
  (raise-minor-mode 'lispy-mode)
  (map! :map lispy-mode-map
        :i "C-d" 'lispy-delete)
  (map! :map lispy-mode-map
        :i "C-k" 'lispy-kill)
  (map! :map lispy-mode-map
        :i "C-y" 'lispy-yank)
  (lispy-define-key lispy-mode-map "X" 'andre/lispy-cider-pprint)
  ;; (lispy-define-key lispy-mode-map "G" 'andre/cider-clojuredocs)
  (lispy-define-key lispy-mode-map "G" 'apt-inf-clojure-doc)
  (lispy-define-key lispy-mode-map "S" 'lispy-move-right)
  (lispy-define-key lispy-mode-map "W" 'lispy-move-left)
  ;; (lispy-define-key lispy-mode-map "J" 'lispy-down-slurp)
  (lispy-define-key lispy-mode-map "K" 'apt-lispy-describe)
  (lispy-define-key lispy-mode-map "O" 'counsel-imenu)
  (lispy-define-key lispy-mode-map "g" 'apt-lispy-g))

(after! clojure
  (map! :map clojure-mode-map
        :localleader
        :desc "Align sexp"
        "f l" #'clojure-align))

(after! cider
  (setq cider-auto-mode nil)
  (map! :map cider-repl-mode-map
        :localleader
        "s" #'cider-switch-to-last-clojure-buffer))

(after! inf-clojure
  (setq inf-clojure-custom-repl-type 'clojure)
  (defun apt-inf-clojure-run-tests ()
    (interactive)
    (inf-clojure-eval-string "(clojure.test/run-tests)"))
  (defun apt-inf-clojure-run-all-tests ()
    (interactive)
    (inf-clojure-eval-string "(clojure.test/run-all-tests)"))

  ;; TODO: Make inf-clojure repl sticky
  ;; (set-popup-rules!
  ;;   '(("^\\*inf-clojure" :quit nil :ttl nil)))
  (map! (:localleader
         (:map (clojure-mode-map clojurescript-mode-map)
          "'" #'inf-clojure
          "c" #'apt-inf-clojure-connect
          "m" #'inf-clojure-macroexpand
          "M" #'cider-macroexpand-all
          (:prefix ("e" . "eval")
           "b" #'inf-clojure-eval-buffer
           "d" #'inf-clojure-eval-defun
           "D" #'inf-clojure-eval-defun-and-go
           "e" #'inf-clojure-eval-last-sexp
           "r" #'inf-clojure-eval-region
           "R" #'inf-clojure-eval-region-and-go)
          (:prefix ("h" . "help")
           "a" #'inf-clojure-apropos)
          (:prefix ("t" . "test")
           "t" #'apt-inf-clojure-run-tests
           "a" #'apt-inf-clojure-run-all-tests)
          (:prefix ("r" . "repl")
           "n" #'inf-clojure-set-ns
           "q" #'inf-clojure-quit
           "R" #'inf-clojure-restart
           "b" #'inf-clojure-switch-to-repl
           ;; TODO: implement for inf clojure
           ;; "B" #'+clojure/cider-switch-to-repl-buffer-and-switch-ns
           "c" #'inf-clojure-clear-repl-buffer))
         ;; TODO: repl bindings
         )))

(after! eglot
  (add-to-list 'eglot-server-programs '(clojure-mode . ("clojure-lsp")))
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-connect-timeout 300))

(after! flycheck
  (map! :leader
        :desc "Next error" "c ]" #'flycheck-next-error)
  (map! :leader
        :desc "Previous error" "c [" #'flycheck-previous-error))

(after! flymake
  (map! :leader
        :desc "Next error" "c ]" #'flymake-goto-next-error)
  (map! :leader
        :desc "Previous error" "c [" #'flymake-goto-prev-error))

(after! magit
  (setq git-commit-style-convention-checks
        (remove 'overlong-summary-line git-commit-style-convention-checks)))

(after! company
  (map! :map company-active-map
        :g "C-h" 'evil-delete-backward-char))

(after! embark
  (map! :map embark-meta-map
        :g "C-a" 'embark-keymap-help))

(after! evil-snipe
  ;; use the 's' key for `evil-substitute` instead of `evil-snipe`
  ;; `evil-snipe` highlights still work
  (evil-snipe-mode -1))

(after! projectile
  ;; used by `projectile-discover-projects-in-search-path'
  (setq projectile-project-search-path '("~/dev/nu"))
  (map! :leader
        :desc "Toggle impl and test"
        "p t" #'projectile-toggle-between-implementation-and-test)
  (map! :leader
        :desc "Replace literal string"
        "p %" #'projectile-replace))

(after! swiper
  (map! :leader
        :desc "counsel-yank-pop"
        "y" #'counsel-yank-pop))

(after! browse-at-remote
  ;; Permanent SHA link.
  (setq browse-at-remote-prefer-symbolic nil))

(after! lsp-mode
  (setq lsp-file-watch-ignored-directories (append lsp-file-watch-ignored-directories
                                                   nu-lsp-ignore-dirs))
  (setq lsp-ui-sideline-show-code-actions nil))
;; TODO: won't work
;; (after! markdown-mode
;;   (setq markdown-header-scaling t))

;; conflicts with lispy
;; (after! evil
;;   (map! :n "C-k" 'kill-line))

;; adapted from https://stackoverflow.com/questions/14760567/emacs-auto-load-color-theme-by-time
(defun synchronize-theme ()
  (let* ((light-theme 'doom-one-light)
         (dark-theme 'doom-one)
         (start-time-light-theme 6)     ; inclusive
         (end-time-light-theme 17)      ; inclusive
         (hour (string-to-number (substring (current-time-string) 11 13)))
         (next-theme (if (member hour (number-sequence start-time-light-theme end-time-light-theme))
                         light-theme dark-theme)))
    (when (not (equal doom-theme next-theme))
      (setq doom-theme next-theme)
      (load-theme next-theme))))

(run-with-timer 0 200 'synchronize-theme)

