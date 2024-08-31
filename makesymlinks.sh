#!/bin/bash

set -o pipefail
mkdir -p ~/.config/karabiner
ln -s ~/dev/peric/dotfiles/karabiner.json ~/.config/karabiner/karabiner.json

ln -s ~/dotfiles/.vimrc ~/.vimrc
ln -s ~/dev/peric/dotfiles/emacs.el ~/.emacs.el

mkdir -p ~/.hammerspoon/
ln -s ~/dev/peric/dotfiles/hammerspoon/init.lua ~/.hammerspoon/init.lua



