# nvm stuff -- see https://formulae.brew.sh/formula/nvm
export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && . "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && . "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

alias ls='ls -G'
alias ll='ls -lG'

# Antigen - the plugin manager for zsh (https://github.com/zsh-users/antigen)
source /opt/homebrew/share/antigen/antigen.zsh

antigen use oh-my-zsh

antigen bundle git

antigen theme theunraveler
antigen apply

# To prevent multiple panes/tabs from sharing the command history.
# See https://superuser.com/questions/1245273/iterm2-version-3-individual-history-per-tab
unsetopt inc_append_history
unsetopt share_history

