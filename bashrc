#!/bin/bash

function working_dir {
    pwd | sed 's/\/usr\/local\/google\/home\/dubroy\/chromium/chromium/g'
}

export PS1="\w\$ "
export PATH="$PATH:~/bin"
export EDITOR='emacs -nw'

PLATFORM=$(uname)

if [[ $PLATFORM == 'Darwin' ]]; then
    alias ls='ls -G'
elif [[ $PLATFORM == 'Linux' ]]; then
    alias ls='ls --color'
fi

# Every time this file is sourced, check the status of the dotfiles repo,
# and remind me if there are uncommited changes.
DOTFILES_STATUS=`cd ~/dotfiles; git status --porcelain; cd -`
if [ -n "$DOTFILES_STATUS" ]; then
    echo "WARNING: Uncommitted changes in ~/dotfiles:"
    echo $DOTFILES_STATUS
fi

if [[ $PLATFORM == 'Linux' ]]; then
    export CCACHE_DIR=~/dev/.ccache
    export CC='ccache gcc'
    export CXX='ccache g++'
fi

# Some Chromium-specific stuff.

alias n="ninja -C out/Debug"
export PATH="$PATH:~/dev/depot_tools:"

# From http://code.google.com/p/chromium/wiki/LinuxFasterBuilds
export GYP_DEFINES="remove_webcore_debug_symbols=1 disable_nacl=1 enable_svg=0"
export GYP_GENERATORS="make,ninja"
