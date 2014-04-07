# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# ok in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="agnoster"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

DISABLE_AUTO_UPDATE="true"
DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git dircycle git-extras git-flow lein mvn vagrant)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
alias TIMESTAMP='date +%Y%m%d-%H%M'
alias dir='ls -lrt'
alias fcl='ack --CLJ'
alias gau='git add -u'
alias autocljs='lein cljsbuild auto'

peek(){
    find $1 -depth 1
}

# Show the numbers for the top ten items in the dirs command
alias all-dirs="dirs|tr ' ' '
'|cat -n| head -10"

alias DATE='date +%Y%m%d'
