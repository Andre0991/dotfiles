# z
. /usr/local/etc/profile.d/z.sh

# better bash history (from https://sanctum.geek.nz/arabesque/better-bash-history/)
HISTFILESIZE=1000000           # default maximum number of commands
HISTSIZE=1000000	       # same
HISTIGNORE='ls:bg:fg:history'  # don't store those commands
shopt -s histappend            # append instead of replacing old entries
