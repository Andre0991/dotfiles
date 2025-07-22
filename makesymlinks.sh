#!/bin/bash

# Default prefix, can be overridden by first argument
PREFIX=${1:-~/dev/peric}

# set -o pipefail
# mkdir -p ~/.config/karabiner
# ln -s ${PREFIX}/dotfiles/karabiner.json ~/.config/karabiner/karabiner.json

# todo: make this script idempotent, consider prefix dir from home/work

ln -s ${PREFIX}/dotfiles/.vimrc ~/.vimrc
ln -s ${PREFIX}/dotfiles/emacs.el ~/.emacs.el

mkdir -p ~/.hammerspoon/
ln -sf ${PREFIX}/dotfiles/hammerspoon/init.lua ~/.hammerspoon/init.lua
ln -s ${PREFIX}/dotfiles/cursor/mcp.json ~/.cursor/mcp.json


