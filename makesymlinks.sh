#!/bin/bash

# set -o pipefail
# mkdir -p ~/.config/karabiner
# ln -s ~/dev/peric/dotfiles/karabiner.json ~/.config/karabiner/karabiner.json

# todo: make this script idempotent, consider prefix dir from home/work

ln -s ~/dotfiles/.vimrc ~/.vimrc
ln -s ~/dev/dotfiles/emacs.el ~/.emacs.el

mkdir -p ~/.hammerspoon/
ln -sf ~/dev/dotfiles/hammerspoon/init.lua ~/.hammerspoon/init.lua



