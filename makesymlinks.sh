#!/bin/bash
# Modified version of Michael Smalley's script
# Source: http://blog.smalleycreative.com/tutorials/using-git-and-github-to-manage-your-dotfiles/
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

git clone https://github.com/Andre0991/dotfiles .

ln -s ~/dotfiles/.spacemacs ~/.spacemacs
ln -s ~/dotfiles/.vimrc ~/.vimrc
ln -s ~/dotfiles/.doom.d ~/.doom.d
