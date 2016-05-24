# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
export DOTFILES=~/dotfiles

ZSH_THEME="robbyrussell"
DISABLE_AUTO_UPDATE="true"
DISABLE_AUTO_TITLE="true"
plugins=(git dircycle gem git-extras git-flow lein mvn osx dirpersist)

source $ZSH/oh-my-zsh.sh

export LANG=en_IE.UTF-8
alias TIMESTAMP='date +%Y%m%d-%H%M'

alias dir='ls -lrt'
alias fcl='ack --CLJ'
alias gau='git add -u'
alias gst='git status'
alias g='git'
alias d='docker'

settitle(){
    echo -ne "\033]0;$*\007"
}

peek(){
    find $1 -depth 1
}
export PATH=/usr/local/bin:~/bin:$PATH

see_line(){
    head -$2 $1 | tail -1
}

source $HOME/zaw/zaw.zsh
bindkey '^x' zaw
bindkey '^r' zaw-history
bindkey '^z' zaw-bookmark

export LESS=-R-X
alias DATE='date +%Y%m%d'

zu () {
    z=$1
    zip -qr $(DATE)-$z $z
}

zm () {
    z=$1
    zip -qrm $(DATE)-$z $z
}

qc(){
    python -c "from math import *; print $1"
}

alias vs='vagrant status'
alias gl='git lola'
alias gg='git grab'
alias gp='git pull'
alias gio='git onto'
alias gco='git checkout'
alias gcob='git checkout -B'
alias gcot='git checkout -T'
alias gdfh='git dfh'
alias greset-soft='git reset --soft'

alias npm-exec='PATH=$(npm bin):$PATH'

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

export EDITOR='emacs --no-init'

HOSTFILE=${DOTFILES}/hosts/$(hostname)
if [ -f  $HOSTFILE ] ; then
    echo "Loading ${HOSTFILE}"
    source $HOSTFILE
fi
