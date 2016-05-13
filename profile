# .profile contains configuration for interactive login shells.
# On Mac OS this includes every new Terminal window.

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

[[ -r ~/.bashrc ]] && . ~/.bashrc
