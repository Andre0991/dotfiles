#!/bin/bash
# Modified version of Michael Smalley's script
# Source: http://blog.smalleycreative.com/tutorials/using-git-and-github-to-manage-your-dotfiles/
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

# check git installation
if ! which git > /dev/null; then
	echo "This script requires git. Please install it."
	exit 1
fi

# Variables
dir=~/dotfiles                    # dotfiles directory
olddir=~/dotfiles_old             # old dotfiles backup directory
files="vimrc vimperatorrc tmux.conf emacs"			  # list of files/folders to symlink in homedir

# create dotfiles_old in homedir
echo -n "Creating $olddir for backup of any existing dotfiles in ~ ..."
mkdir -p $olddir
cd $dir

git clone https://github.com/Andre0991/dotfiles .

# move any existing dotfiles in homedir to dotfiles_old
# directory, then create symlinks from the homedir # to 
# any files in the ~/dotfiles directory specified in $files
for file in $files; do
    echo "Moving any existing dotfiles from ~ to $olddir"
    mv ~/.$file ~/dotfiles_old/
    echo "Creating symlink to $file in home directory."
    ln -s $dir/$file ~/.$file
done

# install Vundle
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

# install tmux
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# extracted from https://github.com/tmux-plugins/tpm/wiki/Installing-plugins-via-the-command-line-only
# start a server but don't attach to it
tmux start-server
# create a new session but don't attach to it either
tmux new-session -d
# install the plugins
~/.tmux/plugins/tpm/scripts/install_plugins.sh
# killing the server is not required
tmux kill-server

# emacs
# ln -s ~/dotfiles/emacs ~/.emacs.d/emacs
# ln -s ~/dotfiles/emacs_andre.org ~/.emacs.d/emacs_andre.org
