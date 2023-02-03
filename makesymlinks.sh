#!/bin/bash

set -o pipefail

mkdir -p ~/dev/peric
cd ~/dev/peric
git clone https://github.com/Andre0991/dotfiles .

ln -s ~/dotfiles/.vimrc ~/.vimrc
ln -s ~/dev/peric/dotfiles/karabiner.json ~/.config/karabiner/karabiner.json
ln -s ~/dev/peric/dotfiles/emacs.el ~/.emacs.el
