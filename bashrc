#!/bin/bash

function working_dir {
    pwd | sed 's/\/usr\/local\/google\/dubroy\/chromium/chromium/g'
}

export PS1="\$(working_dir)\$ "
export PATH="$PATH:~/bin"

alias n="ninja -C out/Debug"

PLATFORM=$(uname)

if [[ $PLATFORM == 'Darwin' ]]; then
    alias ls='ls -G'
elif [[ $PLATFORM == 'Linux' ]]; then
    alias ls='ls --color'
fi

DOTFILES_STATUS=`hg status ~/dotfiles`
if [ -n "$DOTFILES_STATUS" ]; then
    echo "WARNING: Uncommitted changes in ~/dotfiles:"
    echo $DOTFILES_STATUS
fi
