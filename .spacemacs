;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; auto-completion
     ;; better-defaults
     emacs-lisp
     themes-megapack
     (auto-completion :variables
                       auto-completion-enable-help-tooltip t
                       auto-completion-enable-sort-by-usage t
                       auto-completion-enable-snippets-in-popup t
                       )
     ;; git
     ;; markdown
     org
     osx
     syntax-checking
     scala
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; syntax-checking
     version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(org-plus-contrib)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(evil-search-highlight-persist)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         solarized-light)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ","
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; Default value is `cache'.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  )

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;; Mac OS specificities (todo: change according to OS):
  ;; -   (let* ((indir (expand-file-name "~/Dropbox/Screenshots"))
  ;; -   layer osx


  ;; Relative numbers
  ;; Use before loading any file.
  (add-to-hooks 'linum-mode '(org-mode-hook prog-mode-hook))
  (add-to-hooks 'linum-relative-toggle '(org-mode-hook prog-mode-hook))

  ;; Truncate lines
  (add-to-hooks 'spacemacs/toggle-truncate-lines '(org-mode-hook))

  ;; Automatically open some files
    (find-file "~/Dropbox/ciencia_da_computacao/org/notes_computer_science.org")
    (find-file "~/Dropbox/org/todo.org")
    (find-file "~/Dropbox/music_theory/notes_music_theory.org")

    ;; UFABC subjects
    (find-file "~/Dropbox/UFABC/8_quad/evolucao_dos_conceitos_matematicos/notes_evolucao_dos_conceitos_matematicos.org")
    (find-file "~/Dropbox/UFABC/8_quad/paradigmas_de_programacao/notes_paradigmas_de_programacao.org")
    (find-file "~/Dropbox/UFABC/8_quad/programacao_matematica/notes_programacao_matematica.org")
    (find-file "~/Dropbox/UFABC/8_quad/sistemas_operacionais/sistemas_operacionais_notes.org")
    (find-file "~/Dropbox/UFABC/8_quad/tbq_bioquimica/tbq_bioquimica_notes.org")
    (find-file "~/Dropbox/UFABC/8_quad/teoria_dos_grafos/teoria_dos_grafos_notes.org")

    ;; (find-file "~/Dropbox/english/notes_english_grammar_in_use.org")
    ;; (find-file "~/Dropbox/ciencia_da_computacao/practical_vim/notes_practical_vim.org")

   (switch-to-buffer "*spacemacs*")

   ;; Spelling
   (setq ispell-dictionary "british")

   ;; Holidays
   ;; Do not use all default holidays.
   (setq holiday-christian-holidays nil)
   (setq holiday-hebrew-holidays nil)
   (setq holiday-islamic-holidays nil)
   (setq holiday-bahai-holidays nil)
   (setq holiday-oriental-holidays nil)

   ;; Evil leader keybindings

   (evil-leader/set-key
     ;; files
     "oft" '(lambda () (interactive) (find-file "~/Dropbox/org/todo.org"))
     "ofm" '(lambda () (interactive) (find-file "~/Dropbox/org/music_download_and_audition.org"))
     "ofb" '(lambda () (interactive) (find-file "~/Dropbox/org/books.org"))
     ;; org
     "oa" 'org-agenda
     "oc" 'org-capture
     "od" 'org-drill
     "oof" 'org-capture-finalize
     "o." 'org-time-stamp
     "ol" 'helm-locate
     "om" 'helm-semantic-or-imenu
     ;; helm
     "oho" 'helm-occur
     )


   (evil-leader/set-key-for-mode 'org-mode
     "oii" 'insert-org-image
     "oti" 'org-toggle-inline-images
     )
   ;; Org

   ;; Make org-mode consider the line above the image path indicating its
   ;; size and use it inline and when exporting.
   (setq org-image-actual-width nil)

   ;; Pretify Special symbols as UTF-8 characters
   ;; (setq org-pretty-entities t)

   ;; org-drill

   ;; From the org-drill manual: "By default, you will be prompted to save all unsaved buffers at the end of a drill session. If you don't like this behaviour, use the following setting:"
   (setq org-drill-save-buffers-after-drill-sessions-p nil)

   ;; The lower, the more frequently items appear. Default is 0.5.
   (setq org-drill-learn-fraction 0.4)

   ;; org-capture
   (setq org-default-notes-file "~/Dropbox/org/todo.org")

   (setq org-capture-templates
         '(("t" "Todo" entry (file+headline "~/Dropbox/org/todo.org" "Tasks")
            "* TODO %?\n  %i")
           ("m" "Music download" entry (file+headline "~/Dropbox/org/music_download_and_audition.org" "Download")
            "* TODO Download \n Composer: %? \n Work: \n Link: %i")
           ("b" "Books" entry (file+headline "~/Dropbox/org/books.org" "Unclassified")
            "* TODO Read \n Author: %? \n Title: \n Link: %i")
           ("e" "Emacs task" checkitem (file+headline "~/Dropbox/org/todo.org" "Emacs tasks")
            "- [ ] %?\nEntered on %U\n  %i")))
   ;; org-agenda
   (setq org-agenda-files (quote ("~/Dropbox/org/todo.org")))


   ;; smartparens
   (smartparens-global-mode t)

   ;; Functions
   ;; Insert image from Screenshot's folder to org-mode buffer
   ;; Thanks to finster from #emacs on freenode for providing that code. I added the insertion and newline.
(defun get-newest-file-from-dir (dir)
  "Return the file name of the newes file in DIR."
  (when (file-directory-p dir)
    (expand-file-name (car (split-string (shell-command-to-string (format "ls -t %s | head -1" dir)) "\n" t))
                      dir)))





(defun insert-org-image ()
  "Moves image from Dropbox folder to ./media, inserting org-mode link"
  (interactive)
  (let* ((indir (expand-file-name "~/Dropbox/Screenshots"))
         (infile (get-newest-file-from-dir indir))
         (outdir (concat (file-name-directory (buffer-file-name)) "/media"))
         (outfile (expand-file-name (file-name-nondirectory infile) outdir)))
    (unless (file-directory-p outdir)
      (make-directory outdir t))
    (rename-file infile outfile)
    ;; working
  ;; (insert (concat (concat "[[./media/" (file-name-nondirectory outfile)) ".png]]")))
    (insert (concat (concat "[[./media/" (file-name-nondirectory outfile)) "]]")))
  ;; (insert (concat (concat "[[./media/" (file-name-nondirectory outfile)) ".png]]"))
  (newline)
  (newline)
  )



;; check last mispelled word
(evil-define-key 'normal org-mode-map
  (kbd "[ s") 'flyspell-check-previous-highlighted-word
  )

;; swap wv and wV
(evil-leader/set-key
  "wv" 'split-window-right-and-focus
  "wV" 'split-window-right
  "ws" 'split-window-below-and-focus
  "wS" 'split-window-below
  )

;; macros

(evil-set-register ?q  [?i ?* ?* ?  ?Q ?u ?e ?s ?t ?i ?o ?n return return ?* ?* ?* ?  ?A ?n ?s ?w ?e ?r escape ?k])

(evil-set-register ?s  [?i ?< ?s tab ?S ?c ?a ?l ?a escape ?j])

;; do not ask question about following symlinks
(setq vc-follow-symlinks t)

(defun study-emacs-lisp ()
(interactive)
(split-window-right-and-focus)
(split-window-vertically)
(shrink-window-horizontally 10)
(enlarge-window 10)
(find-file "~/Dropbox/ciencia_da_computacao/emacs_lisp.org")
(evil-window-down 1)
(switch-to-buffer "*scratch*")
(emacs-lisp-mode)
(evil-window-left 1)
(bookmark-jump emacs-lisp-intro)
  )

)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(flycheck-scalastyle-jar
   "~/Dropbox/UFABC/8_quad/paradigmas_de_programacao/scala/scalastyle_2.11-0.6.0.jar")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(ring-bell-function (quote ignore) t)
 '(scala-indent:align-forms t)
 '(scala-indent:align-parameters t)
 '(scala-indent:default-run-on-strategy scala-indent:operator-strategy))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
