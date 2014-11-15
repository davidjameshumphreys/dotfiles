# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"
# CASE_SENSITIVE="true"
DISABLE_AUTO_UPDATE="true"
DISABLE_AUTO_TITLE="true"

plugins=(common-aliases lein)
source $ZSH/oh-my-zsh.sh

# User configuration
export PATH=$HOME/bin:/usr/local/bin:$PATH
export LANG=en_IE.UTF-8
source ~/.bash_aliases
source /etc/profile.d/am_dev_env_vars.sh



export WORKON_HOME=~/pyenvs
if [[ -f /usr/share/virtualenvwrapper/virtualenvwrapper.sh ]]; then
    source /usr/share/virtualenvwrapper/virtualenvwrapper.sh
fi

source $HOME/zaw/zaw.zsh
bindkey '^x' zaw
bindkey '^r' zaw-history

export LESS=-R-X
setopt histignoredups

zshaddhistory () {
    # Ignore common commands.
    COMMAND_STR=${1%%$'\n'}
    [[ ( -z $COMMAND_STR ) ||                            \
       ( $COMMAND_STR =~ hist(ory)? ) ||                  \
       ( $COMMAND_STR =~ ^l(s\|l\|a)?$ ) ||                \
       ( $COMMAND_STR =~ ^(d\|gd\|git\ diff\|glp\|gg)$ ) || \
       ( $COMMAND_STR =~ ^gl) ||                             \
       ( $COMMAND_STR =~ ^cd) ||                              \
       ( $COMMAND_STR =~ ^kill)                                \
     ]] && return 1
    return 0
}
