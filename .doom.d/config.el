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
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


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
(setq doom-localleader-key ",")

;; helpers

(defun raise-minor-mode (mode)
    "Make MODE the first on `minor-mode-map-alist'."
    (let ((x (assq mode minor-mode-map-alist)))
      (when x
        (setq minor-mode-map-alist
              (cons x (delq mode minor-mode-map-alist))))))

;; local packages

(let ((nudev-emacs-path "~/dev/nu/nudev/ides/emacs/"))
  (when (file-directory-p nudev-emacs-path)
    (add-to-list 'load-path nudev-emacs-path)
    (require 'nu)))
(load! "~/Dropbox/nu/emacs-lisp/nu-andre.el" nil t)

;; packages

(after! lispy
  (setq lispy-eval-display-style 'overlay)
  ;; M-i for `lispy-iedit' (local) and C-; for iedit (buffer)
  (advice-add 'lispy-iedit :after #'iedit-restrict-function)
  (defun andre/lispy-backward ()
    (interactive)
    (lispy-backward 0)
    (evil-insert 0))
  (defun andre/lispy-forward ()
    (interactive)
    (lispy-forward 1)
    (evil-insert 0))
  ;; prevents `/` (`lispy-splice`) from being overriden
  (raise-minor-mode 'lispy-mode)
  (map! :map lispy-mode-map
        :n "[" 'andre/lispy-backward)
  (map! :map lispy-mode-map
        :n "]" 'andre/lispy-forward)
  (map! :map lispy-mode-map
        :i "C-d" 'lispy-delete)
  (map! :map lispy-mode-map
        :i "C-k" 'lispy-kill))

(after! cider
  ;; https://github.com/clojure-emacs/cider/issues/2808
  (setq cider-enhanced-cljs-completion-p nil))

(after! clojure
  (map! :map clojure-mode-map
        :localleader
        :desc "Align sexp"
        "f l" #'clojure-align))

(after! magit
  (setq git-commit-style-convention-checks
        (remove 'overlong-summary-line git-commit-style-convention-checks)))

(after! company
  (map! :map company-active-map
        :g "C-h" 'evil-delete-backward-char))

(after! evil-snipe
  ;; use the 's' key for `evil-substitute` instead of `evil-snipe`
  ;; `evil-snipe` highlights still work
  (evil-snipe-mode -1))

(after! projectile
  ;; used by `projectile-discover-projects-in-search-path'
  (setq projectile-project-search-path '("~/dev/nu"))
  (map! :leader
        :desc "Toggle impl and test"
        "p t" #'projectile-toggle-between-implementation-and-test))
