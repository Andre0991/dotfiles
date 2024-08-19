#!/bin/bash

set -o pipefail
mkdir -p ~/.config/karabiner
ln -s ~/dotfiles/.vimrc ~/.vimrc
ln -s ~/dev/peric/dotfiles/karabiner.json ~/.config/karabiner/karabiner.json
ln -s ~/dev/peric/dotfiles/emacs.el ~/.emacs.el



