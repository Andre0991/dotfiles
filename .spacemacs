;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'nil
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/Dropbox/backup/emacs/spacemacs/layers/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; helm
     ;; latex
     ;; python
     clojure
     common-lisp
     csv
     emacs-lisp
     epub
     erc
     git
     github
     html
     ivy
     javascript
     markdown
     pdf
     (org :variables
          org-enable-bootstrap-support t
          org-enable-github-support t)
     osx
     ;; pdf
     plantuml
     scala
     shell-scripts
     spacemacs-layouts
     syntax-checking
     themes-megapack
     theming
     version-control
     (shell :variables
            shell-default-shell 'shell)
     (elfeed :variables
             rmh-elfeed-org-files (list "~/Dropbox/backup/emacs/elfeed-feeds.org"))
     (c-c++ :variables
            c-c++-enable-clang-support t)
     (auto-completion :variables
                      auto-completion-private-snippets-directory "~/Dropbox/backup/emacs/yasnippet-snippets/"
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-enable-help-tooltip nil
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup nil
                      :disabled-for latex)
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     enable-flyspell-auto-completion t)
     andre-common-lisp
     andre-elfeed
     andre-epub
     andre-erc
     andre-eww
     andre-ivy
     ;; andre-pdf-tools
     ;; andre-slack
     ;; slack
     )


   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(atomic-chrome
                                      eww-lnum
                                      hyperbole
                                      emidje
                                      deadgrep
                                      w32-browser
                                      worf
                                      wolfram
                                      org-web-tools
                                      beacon
                                      google-this
                                      ix
                                      lispy
                                      exec-path-from-shell
                                      imenu-anywhere
                                      ;; org-mode
                                      org-beautify-theme
                                      ;; org-alert
                                      org-cliplink
                                      org-gcal
                                      ox-epub
                                      org-pdfview
                                      org-variable-pitch
                                      ;; see pre-requisites at https://github.com/jkitchin/ox-clip
                                      ox-clip)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only)

  ;; OS-specific layers
  ;; (when (spacemacs/system-is-mswindows)
  ;;   (append dotspacemacs-configuration-layers
  ;;           '(autohotkey)))
  ;; (when (spacemacs/system-is-mac)
  ;;   (append dotspacemacs-configuration-layers
  ;;           '(osx)))
  )

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(leuven
                         gruvbox)
   ;; If non nil the cursor color matches the state color in GUI Emacs.

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 20
                               :weight normal
                               :width normal
                               ;; :powerline-scale 1.1
                               )
   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers 'relative
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%f"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  ;; Commenting this in favour of exec-path-from-shell - Spacemacs messes with sourced function.
  ;; (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (defun andre/org-insert-question ()
    (interactive)
    (org-insert-heading-after-current)
    (insert "Answer")
    (evil-previous-line)
    (org-insert-heading-after-current)
    (org-promote))

  ;; Variable pitch
  (defun andre/org-mode-readable ()
    (interactive)
    (setq line-spacing 0.1)
    (variable-pitch-mode))

  (spacemacs/add-to-hooks 'org-variable-pitch-minor-mode '(org-mode-hook))
  (spacemacs/add-to-hooks 'andre/org-mode-readable '(markdown-mode-hook)))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  ;; Spacemacs has a mechanism for getting env vars,
  ;; but it didn't actually work in my setup
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  ;; set folders
  (setq andre-type-env (if (file-exists-p "~/.emacs-work") 'work-mac 'home-mac))
  (setq andre--home-todo-path "~/Dropbox/org/todo-home.org")
  (setq andre--work-todo-path "~/Dropbox/nu/org/todo-work.org")
  (cond ((eq andre-type-env 'work-mac)
         (setq andre--path-to-org-todo andre--work-todo-path))
        ((eq andre-type-env 'home-mac)
         (setq andre--path-to-org-todo andre--home-todo-path)))
  (setq andre--path-to-books-file "~/Dropbox/org/books.org")
  (setq andre--workspaces-path "~/Dropbox/backup/emacs/workspaces.el")

  ;; Editing styles
  (define-key evil-hybrid-state-map (kbd "C-w") 'evil-delete-backward-word)

  ;; Set emacs source folder
  (setq find-function-C-source-directory "~/Dropbox/ciencia_da_computacao/emacs/source/emacs-26.1/src")

  ;; My functions
  (defun andre-open-k2pdfopt-shell ()
    (interactive)
    (shell)
    (insert "cd /Users/andreperictavares/Dropbox/kindle/ && ./k2pdfopt")
    (comint-send-input))

  (defun andre/clear-compilation-buffer ()
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (define-key compilation-mode-map (kbd "C-l") 'andre/clear-compilation-buffer)

  ;; modified from http://ergoemacs.org/emacs/elisp_change_space-hyphen_underscore.html
  (defun andre/dash-to-underscore-region (@begin @end)
    "Change dash char to underscore."
    (interactive "r")
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (goto-char (point-min))
        (while
            (re-search-forward "-" (point-max) "NOERROR")
          (replace-match "_" "FIXEDCASE" "LITERAL")))))

  ;; "he said \"hello world\"\" => 'he said "hello world"'
  ;; 'he said "hello world"' => "he said \"hello world\"\"
  (defun andre/swap-literal-quote (@begin @end)
    "Transform string in region from quoted to literal or vice-versa. Region must include the enclosing quotes."
    (interactive "r")
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (goto-char (point-min))
        (let* ((delete-active-region nil)
               (str-is-literal (char-equal ?' (following-char)))
               (new-delimiter-quote (if str-is-literal "\"" "'"))
               (quote-regex "\"")
               (escaped-quote-regex "\\\\\"")
               (quote-to-be-replaced-regex (if str-is-literal quote-regex escaped-quote-regex))
               (new-quote-regex (if str-is-literal escaped-quote-regex quote-regex)))
          (message new-delimiter-quote)
          (delete-forward-char 1)
          (insert new-delimiter-quote)
          (while
              (re-search-forward quote-to-be-replaced-regex (point-max) "NOERROR")
            (replace-match new-quote-regex))
          (goto-char (point-max))
          (delete-backward-char 1)
          (insert new-delimiter-quote)))))

  (defun andre//read-lines (filePath)
    "Return a list of lines of a file at filePath."
    (with-temp-buffer (insert-file-contents filePath) (split-string (buffer-string) "\n" t)))

  (defun andre/convert-file-name-to-my-standard-format (filename)
    "Change given string to lower case and replace , to _."
    (interactive "s")
    (let* ((filename-downcase (downcase filename))
           (filename-without-commas (replace-regexp-in-string "," "" filename-downcase))
           (filename-without-whitespace (replace-regexp-in-string " " "_" filename-without-commas)))
      (message filename-without-whitespace)
      (kill-new filename-without-whitespace)))

  (cl-defun andre/open-pdf (pdf-file &key (execute-enlarge-window-right-horizontally nil execute-enlarge-window-right-horizontally-p) (fit-page nil fit-page-p))
    "Open pdf file in the last bookmarked page."
    (find-file pdf-file)
    (when execute-enlarge-window-right-horizontally-p (spacemacs/enlarge-window-horizontally 15))
    (when fit-page-p (pdf-view-set-slice-from-bounding-box))
    (ignore-errors
      (bookmark-jump (buffer-name))))

  ;; Common Lisp
  (setq inferior-lisp-program "/usr/local/bin/clisp")

  ;; Lispy
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'cider-repl-mode (lambda () (lispy-mode 1)))


  ;; default was message
  ;; (setq lispy-eval-display-style 'message)
  (setq lispy-eval-display-style 'overlay)

  ;; M-i for local scope editing, C-M-i for top level refactoring
  (advice-add 'lispy-iedit :after #'iedit-restrict-function)
  

  ;; imenu-everywhere
  (setq imenu-anywhere-buffer-filter-functions
        '(imenu-anywhere-same-project-p))
  ;; was (imenu-anywhere-same-mode-p imenu-anywhere-friendly-mode-p imenu-anywhere-same-project-p)

  ;; keybindings
  (global-set-key "\C-h" 'delete-backward-char)
  (global-set-key (kbd "C-x C-l") 'evil-complete-next-line)
  (spacemacs/set-leader-keys "p d" 'deadgrep)
  (evil-leader/set-key
    ;; org
    "bS" 'andre-org-scrath-buffer
    "oft" '(lambda () (interactive) (find-file andre--path-to-org-todo))
    "oa"  'org-agenda
    "oc"  'counsel-org-capture
    "oji" 'counsel-org-goto-all
    "ogc" 'org-clock-goto
    "jI"  'imenu-anywhere)
  ;; insert mode
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char)
  ;; company
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
    ;; TODO: won't reload the completions after deleting the char
    (define-key company-active-map (kbd "C-h") 'evil-delete-backward-char)
    ;; was C-M-i (default completion-at-point keybinding)
    (define-key company-active-map (kbd "<tab>") 'completion-at-point))

  ;; ;; Smartparens
  ;; TODO: Buggy on org-mode: = and ~ move the pointer wrongly
  ;; Using `electric-pair-mode` now, which is enough for me
  ;; (smartparens-global-mode t)
  ;; (setq sp-highlight-pair-overlay nil)
  ;; (setq sp-highlight-wrap-overlay nil)
  ;; (setq sp-highlight-wrap-tag-overlay nil)
  (electric-pair-mode 1)

  ;; Misc
  (setq vc-follow-symlinks t) ;; do not ask question about following symlinks
  (setq large-file-warning-threshold nil) ;; doc-view
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  (setq confirm-kill-processes nil) ;; stop asking "Active processes exist; kill them and exit anyway"
  ;; holidays
  (setq holiday-christian-holidays nil)
  (setq holiday-hebrew-holidays nil)
  (setq holiday-islamic-holidays nil)
  (setq holiday-bahai-holidays nil)
  (setq holiday-oriental-holidays nil)

  ;; Truncate lines (if like is greater than screen, break it visually)
  (spacemacs/add-to-hooks 'spacemacs/toggle-truncate-lines-off '(org-mode-hook markdown-mode-hook))
  (add-hook 'text-mode-hook #'toggle-word-wrap)
  (spacemacs/add-to-hooks 'worf-mode '(org-mode-hook))


  ;; Packages misc
  ;; Scheme
  (setq scheme-program-name  "/usr/local/bin/mit-scheme")

  ;; Magit
  ;; see https://magit.vc/manual/magit/Wip-Modes.html
  (magit-wip-after-save-mode)

  ;; Yasnippet
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))

  ;; Aux functions
  (defun andre/indent-buffer ()
    "Indents current buffer"
    (interactive)
    (indent-region (point-min) (point-max)))

  (defun andre//read-lines (filePath)
    "Return a list of lines of a file at filePath."
    (with-temp-buffer (insert-file-contents filePath)
                      (split-string (buffer-string) "\n" t)))

  ;; wolfram
  (let ((acc (andre//read-lines "~/Dropbox/backup/emacs/secrets/wolfram-acc")))
    (setq wolfram-alpha-app-id (car acc)))

  ;; Indent org babel block
  ;; from http://stackoverflow.com/questions/15773354/indent-code-in-org-babel-src-blocks
  (defun andre/org-indent-org-src-block ()
    (interactive)
    (org-edit-special)
    (andre/indent-buffer)
    (org-edit-src-exit))

  ;; Required for buffers with variable pitch - otherwise the number column length will vary
  (set-face-attribute 'line-number nil
                      :font "Source Code Pro"
                      :family "Monospace")
  (set-face-attribute 'line-number-current-line nil
                      :font "Source Code Pro"
                      :family "Monospace")

  ;; From https://github.com/syl20bnr/spacemacs/tree/develop/layers/org :
  ;; "Since version 0.104, spacemacs uses the org version from the org ELPA repository instead of the one shipped with emacs. Then, any org related code should not be loaded before dotspacemacs/user-config, otherwise both versions will be loaded and will conflict.
  ;; Because of autoloading, calling to org functions will trigger the loading up of the org shipped with emacs wich will induce conflicts. One way to avoid conflict is to wrap your org config code in a with-eval-after-load block like this:"
  (with-eval-after-load 'org
    (org-defkey org-mode-map (kbd "}") #'andre-brackets)

    ;; org-download
    ;; macOS command
    (setq org-download-screenshot-method "screencapture -i %s")
    (defun andre-org-download-setup ()
      (setq org-download-image-dir "~/Dropbox/org/images/")
      ;; do not create a folder with the heading name; save all images in the same dir
      (setq org-download-heading-lvl nil))
    (advice-add 'org-download-screenshot :before #'andre-org-download-setup)


    (defun andre/go-to-worf-in-previous-heading ()
      (interactive)
      (worf-backward)
      (evil-insert 0))

    (defun andre/go-to-worf-in-next-heading ()
      (interactive)
      (worf-forward)
      (evil-insert 0))

    (setq org-hide-emphasis-markers t) ;; removes markers such as * in *bold*, / in /italic/ etc
    (require 'ox-md nil t) ;; show markdown in list of exported formats
    ;; From https://github.com/cadadr/elisp/blob/master/org-variable-pitch.el
    (require 'org-variable-pitch)
    (setq org-variable-pitch-fixed-font "Source Code Pro")

    ;; org normal mode keybindings
    (evil-define-key 'normal org-mode-map
      (kbd "[") 'andre/go-to-worf-in-previous-heading
      (kbd "]") 'andre/go-to-worf-in-next-heading)

    ;; org leader keybindings
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "ji"  'counsel-org-goto
      "jI"  'counsel-org-goto-all
      "iL"  'org-cliplink
      "ip"  'andre/paste-html2org-clipboard
      "iq"  'andre/org-insert-question
      "od"  'org-drill
      "oti" 'org-toggle-inline-images
      "o'"  'org-babel-indent-and-quit
      "ocf" 'org-capture-finalize
      "n"   'org-cycle-agenda-files)

    ;; Org
    (setq org-columns-default-format
          "%45ITEM %TODO %3PRIORITY %TIMESTAMP")
    (setq org-startup-indented t)
    (setq org-startup-truncated t) ;; truncate lines (based on window size)
    ;; see http://orgmode.org/manual/Initial-visibility.html
    (setq org-startup-folded "showeverything")
    (setq org-agenda-inhibit-startup nil)
    ;; org-habit
    (setq org-habit-following-days 0)
    (setq org-habit-preceding-days 7)
    (setq org-habit-graph-column 55)
    (setq org-habit-show-all-today t) ;; show completed tasks too

    ;; export options
    (setq org-export-with-toc nil)
    (setq org-export-with-section-numbers nil)

    ;; export only subtree
    (defun andre/org-export-subtree-to-markdown-github ()
      (interactive)
      (let ((org-export-with-toc nil))
        (org-md-export-to-markdown nil t nil)
        (save-excursion
          (let* ((exported-file (org-export-output-file-name ".md")))
            (find-file exported-file)
            (clipboard-kill-ring-save (point-min) (point-max))))))

    ;; save the clock history across Emacs sessions
    (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)
    (spacemacs/toggle-mode-line-org-clock-on)

    ;; Multi-state worflows: http://orgmode.org/guide/Multi_002dstate-workflows.html
    ;; Options for each state:
    ;;   "!" adds timestamp for state logging
    ;;   "@" asks for note when changing to the state
    ;; Adapted from http://doc.norang.ca/org-mode.html
    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d)")
                  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

    ;; By default, ~org-store-link~ returns the name of the heading, which can be stored as a link with ~org-insert-link~, which will insert [[name-of-the-heading]].
    ;; This is not a good approach, because if the header name is modified, the link gets broken.
    ;; Setting this variable to true makes ~org-store-link~ create =:PROPERTIES= (thus, an ID). So we can link to the ID instead of the name of heading - much less fragile.
    (setq org-id-link-to-org-use-id t)


    ;; babel
    ;; List of languages that can be executed with <C-c><C-c>.
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)))

    (setq org-confirm-babel-evaluate nil)
    (setq org-confirm-shell-link-function nil)
    (setq org-confirm-elisp-link-function nil)
    ;; Make org-mode consider the line above the image path indicating its
    ;; size and use it inline and when exporting.
    (setq org-image-actual-width nil)

    ;; org-drill
    ;; "By default, you will be prompted to save all unsaved buffers at the end of a drill session. If you don't like this behaviour, use the following setting:"
    (setq org-drill-save-buffers-after-drill-sessions-p nil)
    ;; The lower, the more frequently items appear. Default is 0.5.
    (setq org-drill-learn-fraction 0.4)

    ;; org-capture
    (setq org-capture-templates
          '(("t" "Todo (Work)" entry (file+headline andre--work-todo-path "Tasks")
             "* TODO %?\n%U\n  %i")
            ("h" "Todo (Home)" entry (file+headline andre--home-todo-path "Tasks")
             "* TODO %?\n%U\n  %i")
            ("l" "Learn" entry (file+headline andre--home-todo-path "Learn")
             "* TODO %?\n%U\n  %i")
            ("m" "Music" entry (file+headline "~/Dropbox/org/music_download_and_audition.org" "Download")
             "* TODO Download\n%U\nComposer: %? \nWork: \nLink: %i")
            ("i" "Ideas" entry (file+headline andre--path-to-org-todo "Ideas")
             "* %? \n%U\n Description: %i")
            ("b" "Books" entry (file+headline andre--path-to-books-file "Unclassified")
             "* TODO %? \n%U\nAuthor: \n [[][Amazon]] %i")
            ("e" "Emacs" entry (file+headline "~/Dropbox/org/emacs.org" "Emacs tasks")
             "* TODO %?\n%U\n %i")))

    ;; org-agenda
    (setq org-default-notes-file andre--path-to-org-todo)
    (setq org-agenda-files `(,andre--home-todo-path ,andre--work-todo-path))

    ;; org-refile
    (setq org-refile-targets `((,andre--home-todo-path :maxlevel . 3)
                               (,andre--work-todo-path :maxlevel . 3)
                               (,andre--path-to-books-file :maxlevel . 3)))

    (setq search-default-mode #'char-fold-to-regexp)

    ;; org agenda
    (setq org-agenda-span 'month)

    (setq org-agenda-custom-commands '(("w" "Work tasks" ((tags-todo "work")
                                                          (org-agenda-prefix-format "$l%t%s")))
                                       ("h" "Home tasks" tags-todo "home")))

    ;; org-pdfview
    ;; from org-pdfview.el
    (require 'org-pdfview)
    ;; Relevant? https://github.com/markus1189/org-pdfview/issues/10
    (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
    (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open))

    ;; from https://github.com/abo-abo/worf/issues/31
    (defun andre-brackets ()
      (interactive)
      (message "hi people")
      (insert "[]")
      (backward-char)))

  ;; occur

  ;; center after selecting item in occur
  ;; https://emacs.stackexchange.com/questions/30516/show-the-matching-line-in-the-center-of-the-screen-instead-of-the-bottom-when-op
  (setq next-error-recenter '(4))

  ;; Cider
  ;; Fuzzy completion
  ;; https://github.com/alexander-yakushev/compliment/wiki/Examples
  (defun andre-cider-hook ()
    (cider-load-file (expand-file-name "lispy-clojure.clj" lispy-site-directory))
    (cider-nrepl-sync-request:eval "(clojure.tools.namespace.repl/set-refresh-dirs \"src\")")
    (cider-ns-refresh))

  (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-connected-hook 'andre-cider-hook)
  ;; (remove-hook 'cider-connected-hook 'andre-cider-hook)

  (setq cljr-middleware-ignored-paths '(".*/test/.*"))

  (setq cider-repl-use-pretty-printing t)

  ;; default was 'both
  (setq cider-use-overlays t)

  (defun andre-cider-clear-buffer-and-eval-top-level-form ()
    (interactive)
    (save-excursion
      (cider-switch-to-repl-buffer)
      (cider-repl-clear-buffer))
    (cider-eval-defun-at-point))

  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
    "et"  'andre-cider-clear-buffer-and-eval-top-level-form)

  ;; make let bindings global (HACKISH)
  (setq andre--catpure-let-binding "(defmacro my-locals [] (let [my-bindings (seq (into {} (map (juxt (comp keyword name) identity)) (keys &env)))] (for [[my-name value] my-bindings] (list 'def (symbol (name my-name)) value))))")
  (defun andre--but-last (str)
    (substring str 0 (- (length str) 1)))
  (defun andre-make-let-bindings-global ()
    (interactive)
    (let* ((b-str (lispy--string-dwim))
           ;; (let [a b] foo) -> (let [a b] foo (my-locals))
           (form-with-macro (concat (andre--but-last b-str) " (my-locals))")))
      (message form-with-macro)
      (cider-nrepl-sync-request:eval andre--catpure-let-binding)
      (cider-nrepl-sync-request:eval form-with-macro)))

  ;; Lispy

  ;; lispy-mark-symbol is off by one char if we don't get to insert mode first
  (defun andre-evil-insert-lispy ()
    (evil-insert 0))
  (advice-add 'lispy-mark-symbol :before #'andre-evil-insert-lispy)

  ;; From https://oremacs.com/2014/12/31/keymap-arms-race/
  ;; Prevent '/' (lispy-splice) from being overriden
  (defun raise-minor-mode (mode)
    "Make MODE the first on `minor-mode-map-alist'."
    (let ((x (assq mode minor-mode-map-alist)))
      (when x
        (setq minor-mode-map-alist
              (cons x (delq mode minor-mode-map-alist))))))

  (defun andre/raise-lispy-minor-mode ()
    (raise-minor-mode 'lispy-mode))

  (spacemacs/add-to-hook
   'clojure-mode-hook
   '(andre/raise-lispy-minor-mode))

  ;; default was `find-file-in-project` (https://github.com/technomancy/find-file-in-project)
  (defun andre/display-ansi-colours ()
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  (setq lispy-visit-method 'projectile-find-file)
  ;; lispy keybindings
  (defun andre/lispy-backward-and-go-to-insert-mode ()
    (interactive)
    (lispy-backward 0)
    (evil-insert 0))
  (defun andre/lispy-forward-and-go-to-insert-mode ()
    (interactive)
    (lispy-forward 1)
    (evil-insert 0))
  ;; 'g' requires tags setup, so let's just use imenu instead

  (defun andre/lispy-imenu-fallback ()
    (interactive)
    (call-interactively 'counsel-imenu)
    ;; (if (eql major-mode 'clojure-mode)
    ;;     (call-interactively 'imenu)
    ;;   (lispy-goto-mode))
    )

  ;; Midje messages use ansi colours
  (defun andre/lispy-coloured-message (orig-fun &rest args)
    ;; TODO: check clojure mode!
    (if (or (> (length (car args)) 4000) (> (cl-count ?\n (car args)) (or 14 (* (window-height (frame-root-window)) max-mini-window-height))))
        ;; using ansi-color-apply won't work for all colours
        (progn
          (apply orig-fun args)
          (with-current-buffer (pop-to-buffer "*lispy-message*")
            (andre/display-ansi-colours)))
      (progn
        (apply orig-fun (list (ansi-color-apply (car args)))))))

  (defun andre-lispy-pprint ()
    (interactive)
    (lispy-different)
    (cider-pprint-eval-last-sexp)
    (lispy-different))


  (eval-after-load "lispy"
    `(progn (advice-add 'lispy-message :around #'andre/lispy-coloured-message)
            (lispy-define-key lispy-mode-map "g" 'andre/lispy-imenu-fallback)
            (lispy-define-key lispy-mode-map "v" 'evil-scroll-line-to-center)
            (lispy-define-key lispy-mode-map "X" 'andre-lispy-pprint)
            (lispy-define-key lispy-mode-map "K" 'lispy-up-slurp)
            (lispy-define-key lispy-mode-map "J" 'lispy-down-slurp)
            (lispy-define-key lispy-mode-map "W" 'lispy-move-left)
            (lispy-define-key lispy-mode-map "S" 'lispy-move-right)
            (lispy-define-key lispy-mode-map "I" 'cider-inspect-last-result)
            (define-key lispy-mode-map (kbd "C-M-i") 'evil-iedit-state/iedit-mode)
            ;; originally M-n, which clashes with Spacemacs
            (define-key lispy-mode-map (kbd "M-l") 'lispy-mark-symbol)))

  (evil-define-key 'normal lispy-mode-map
    (kbd "[") 'andre/lispy-backward-and-go-to-insert-mode
    (kbd "]") 'andre/lispy-forward-and-go-to-insert-mode)

  ;; from https://stackoverflow.com/questions/23378271/how-do-i-display-ansi-color-codes-in-emacs-for-any-mode

  ;; git link
  (setq git-link-use-commit t)

  ;; org-mode rich pasting (OSX only)
  ;; https://xiangji.me/2015/07/13/a-few-of-my-org-mode-customizations/
  ;; https://emacs.stackexchange.com/questions/12121/org-mode-parsing-rich-html-directly-when-pasting
  (defun andre/paste-html2org-clipboard ()
    "Convert clipboard contents from HTML to Org and then paste (yank)."
    (interactive)
    (kill-new (shell-command-to-string "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t json | pandoc -f json -t org --wrap=none"))
    (yank)
    ;; replace Unicode Character 'NO-BREAK SPACE'
    (save-excursion
      (goto-char 1)
      (while (re-search-forward "\u00A0" nil t)
        (replace-match " " nil nil))))

  ;; atomic-chrome - https://github.com/alpha22jp/atomic-chrome/blob/master/README.md
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-url-major-mode-alist '(("\\.cloud\\.databricks\\.com" . scala-mode)
                                             ("metabase.*\\.com" . sql-mode)
                                             ("github\\.com" . gfm-mode)))
  (atomic-chrome-start-server)

  (when (file-exists-p andre--workspaces-path)
    (load-file andre--workspaces-path))

  ;; ix (pastebin like service for sharing snippets of code)
  (defalias 'andre/pastebin-share-snippet 'ix)

  ;; org table
  ;; `org-table-create-or-convert-from-region` uses a different number of the
  ;; universal prefix for using different separator. Remembering that is just beyound human ability
  (defalias 'andre/create-org-table-using-space-separator
    '(lambda ()
       (interactive)
       (call-interactively 'org-table-create-or-convert-from-region)))

  (defalias 'andre/create-org-table-using-comma-separator
    '(lambda ()
       (interactive)
       (let ((current-prefix-arg '(4)))
         (call-interactively 'org-table-create-or-convert-from-region))))

  (defalias 'andre/create-org-table-using-tab-separator
    '(lambda ()
       (interactive)
       (let ((current-prefix-arg '(16)))
         (call-interactively 'org-table-create-or-convert-from-region))))


  (defalias 'andre/create-org-table-using-regex-separator
    '(lambda ()
       (interactive)
       (let ((current-prefix-arg '(64)))
         (call-interactively 'org-table-create-or-convert-from-region))))

  ;; nu
  (let ((nudev-emacs-path "~/dev/nu/nudev/ides/emacs/"))
    (when (file-directory-p nudev-emacs-path)
      (add-to-load-path nudev-emacs-path)
      (require 'nu)))

  (let ((nu-coll-path "~/dev/nu/playbooks/squads/collections/dev/emacs/"))
    (when (file-directory-p nu-coll-path)
      (add-to-load-path nu-coll-path)
      (require 'nu-collections)))

  (when (file-exists-p "~/Dropbox/nu/emacs-lisp/nu-andre.el")
    (load-file "~/Dropbox/nu/emacs-lisp/nu-andre.el"))

  ;; dired
  (evil-define-key 'normal dired-mode-map
    (kbd "[") 'dired-up-directory)
  (defun andre-dired-mode-hide-details ()
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'andre-dired-mode-hide-details)

  ;; ivy
  ;; requires lexical binding (set at the top of the file)
  (defun andre/counsel-rg-src ()
    (interactive)
    (let ((search-str (ivy--input)))
      ;; https://emacs.stackexchange.com/questions/20974/exit-minibuffer-and-execute-a-command-afterwards
      (ivy-quit-and-run
        (counsel-rg search-str (projectile-project-root) "--iglob '!test*' --iglob '!postman*'"))))

  (defun andre/counsel-rg-src-and-tests ()
    (interactive)
    (let ((search-str (ivy--input)))
      ;; https://emacs.stackexchange.com/questions/20974/exit-minibuffer-and-execute-a-command-afterwards
      (ivy-quit-and-run
        (counsel-rg search-str (projectile-project-root)))))

  (define-key ivy-minibuffer-map (kbd "C-s") #'andre/counsel-rg-src)
  (define-key ivy-minibuffer-map (kbd "C-t") #'andre/counsel-rg-src-and-tests)

  ;; shell
  (define-key shell-mode-map (kbd "C-l") #'comint-clear-buffer)
  (define-key shell-mode-map (kbd "C-r") #'counsel-shell-history)

  ;; eshell
  ;; this has to be defined within the hook as eshell-mode-map doesn't exist before that
  (add-hook 'eshell-mode-hook #'(lambda ()
                                  ;; TODO: insert keybindings won't work
                                  (evil-define-key 'normal eshell-mode-map (kbd "C-r") 'helm-eshell-history)
                                  (evil-define-key 'insert eshell-mode-map (kbd "C-r") 'helm-eshell-history)
                                  (evil-define-key 'normal eshell-mode-map (kbd "C-j") 'eshell-next-matching-input-from-input)
                                  (evil-define-key 'insert eshell-mode-map (kbd "C-j") 'eshell-next-matching-input-from-input)
                                  (evil-define-key 'normal eshell-mode-map (kbd "C-k") 'eshell-previous-matching-input-from-input)
                                  (evil-define-key 'insert eshell-mode-map (kbd "C-k") 'eshell-previous-matching-input-from-input)))

  ;; temp buffer
  (defun andre-org-scrath-buffer ()
    (interactive)
    (find-file "~/Dropbox/temp/org-temp-buffer.org"))

  ;; projectile
  (with-eval-after-load 'projectile
    (projectile-register-project-type 'lein-test '("project.clj")
                                      :compile "lein compile"
                                      :test "lein nu-test :autotest"
                                      :run "lein run"
                                      :test-suffix "_test"))
  ;; default files
  (when (eq andre-type-env 'work-mac)
    (find-file "~/Dropbox/nu/org/todo-work.org")
    (find-file "~/Dropbox/nu/org/tech.org")
    (find-file "~/Dropbox/ciencia_da_computacao/datomic/datomic-notes.org")
    (find-file "/Users/andreperictavares/Dropbox/ciencia_da_computacao/programming_languages/bash/notes_bash.org")))

 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (emidje zenburn-theme zen-and-art-theme yasnippet-snippets xterm-color ws-butler worf wolfram winum white-sand-theme which-key wgrep web-mode web-beautify w32-browser volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon sx sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smex smeargle slime-company slim-mode shr-tag-pre-highlight shell-pop seti-theme scss-mode sass-mode reverse-theme reveal-in-osx-finder restart-emacs rebecca-theme rainbow-delimiters railscasts-theme purple-haze-theme pug-mode professional-theme prettier-js plantuml-mode planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el password-generator paradox ox-twbs ox-gfm ox-epub ox-clip overseer osx-trash osx-dictionary orgit organic-green-theme org-web-tools org-variable-pitch org-projectile org-present org-pomodoro org-pdfview org-mime org-gcal org-download org-cliplink org-bullets org-brain org-beautify-theme open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme nov noctilux-theme neotree naquadah-theme nameless mvn mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme meghanada maven-test-mode material-theme markdown-toc majapahit-theme magithub magit-svn magit-gitflow magit-gh-pulls madhat2r-theme lush-theme lorem-ipsum livid-mode lispy link-hint light-soap-theme launchctl kaolin-themes json-navigator json-mode js2-refactor js-doc jbeans-theme jazz-theme ix ivy-yasnippet ivy-xref ivy-rtags ivy-purpose ivy-hydra ir-black-theme insert-shebang inkpot-theme indent-guide impatient-mode imenu-anywhere hyperbole hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-make hc-zenburn-theme gruvbox-theme gruber-darker-theme groovy-mode groovy-imports grandshell-theme gradle-mode gotham-theme google-translate google-this google-c-style golden-ratio gnuplot gitignore-templates gitignore-mode github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md gandalf-theme fuzzy font-lock+ flyspell-popup flyspell-correct-ivy flycheck-rtags flycheck-pos-tip flycheck-bashate flx-ido flatui-theme flatland-theme fish-mode fill-column-indicator farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme exec-path-from-shell eww-lnum evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks ensime emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies editorconfig dumb-jump dracula-theme dotenv-mode doom-themes doom-modeline django-theme disaster diminish diff-hl deadgrep darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme csv-mode counsel-projectile counsel-css company-web company-tern company-statistics company-shell company-rtags company-emacs-eclim company-c-headers common-lisp-snippets column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme clojure-snippets clojure-cheatsheet clj-refactor clean-aindent-mode clang-format cider-eval-sexp-fu cherry-blossom-theme centered-cursor-mode busybee-theme bubbleberry-theme browse-at-remote birds-of-paradise-plus-theme beacon badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile atomic-chrome apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
