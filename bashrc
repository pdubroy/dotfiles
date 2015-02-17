#!/bin/bash

# .bashrc contains configuration for interactive shells

export PS1="\w\$ "
export PATH="~/bin:$PATH"
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

# Dynamically add the `npm bin` directory to $PATH.
ORIG_PATH=$PATH
function setPathToNpmBin() {
  export PATH=$ORIG_PATH
  SEARCH_DIR=$(pwd)
  # Search for the bin directory. Like `npm bin`, but faster.
  while true; do
    NPM_BIN="$SEARCH_DIR/node_modules/.bin"
    [[ -d "$NPM_BIN" ]] && export PATH="$NPM_BIN:$ORIG_PATH" && break
    [[ "$SEARCH_DIR" == "/" ]] && break
    SEARCH_DIR="$(dirname "$SEARCH_DIR")"
  done
}
PROMPT_COMMAND=setPathToNpmBin
