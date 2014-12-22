export NVM_DIR="/Users/dubroy/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

VIRTUALENV=~/venv/base/bin/activate
[ -r "$VIRTUALENV" ] && source "$VIRTUALENV"

[[ -r ~/.bashrc ]] && . ~/.bashrc
