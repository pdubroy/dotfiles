# .bashrc contains configuration for interactive, non-login shells.

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

export NVM_DIR="/Users/dubroy/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

VIRTUALENV=~/.venv/base/bin/activate
[ -s "$VIRTUALENV" ] && source "$VIRTUALENV"  # Activate virtualenv.

# Dynamically add the `npm bin` directory to $PATH.
# TODO: This has the problem that any modifications to PATH after this file is
# sourced will be lost every time this function is re-executed.
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
