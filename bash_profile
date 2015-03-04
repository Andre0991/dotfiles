# iTerm2 -  run 'title example' to make 'example' the title of current session
function title {
    echo -ne "\033]0;"$*"\007"
}

# Homebrew path comes first
export PATH="/usr/local/bin:$PATH"

# coreutils
PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"

# GNU sed
PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
MANPATH="/usr/local/opt/gnu-sed/libexec/gnuman:$MANPATH";

# Add SWI-Prolog to the path
export PATH=/Applications/SWI-Prolog.app/Contents/MacOS:$PATH

# Add GHC 7.8.3 to the path, via http://ghcformacosx.github.io/
export GHC_DOT_APP="/Applications/ghc-7.8.3.app"
if [ -d "$GHC_DOT_APP" ]; then
    export PATH="${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
fi

# Terminal colours
eval $( dircolors -b $HOME/.dir_colors )

# Aliases
alias ls="ls --color=always"
alias grep="grep --color=always"
alias ledger="ledger -f ~/Dropbox/notes/ledger.dat"