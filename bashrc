#!/bin/bash

export PS1="\w\$ "
export PATH="$PATH:~/bin"
export EDITOR='emacs -nw'

PLATFORM=$(uname)

if [[ $PLATFORM == 'Darwin' ]]; then
    alias ls='ls -G'
elif [[ $PLATFORM == 'Linux' ]]; then
    alias ls='ls --color'
fi

alias emacs='emacs -nw'

# Every time this file is sourced, check the status of the dotfiles repo,
# and remind me if there are uncommited changes.
DOTFILES_STATUS=`GIT_DIR=~/dotfiles/.git GIT_WORK_TREE=~/dotfiles git status --porcelain`
if [ -n "$DOTFILES_STATUS" ]; then
    echo "WARNING: Uncommitted changes in ~/dotfiles:"
    echo $DOTFILES_STATUS
else
    echo "Updating dotfiles..."
    GIT_DIR=~/dotfiles/.git GIT_WORK_TREE=~/dotfiles git pull
fi
