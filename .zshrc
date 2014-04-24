OS=$(uname)

# path
export PATH=/usr/local/bin:$PATH
export PATH=$PATH:/usr/local/sbin:/usr/libexec:/opt/local/sbin
export PATH=~/.cabal/bin:$PATH
export PATH=$PATH:/usr/local/git/libexec/git-core
# Always override with my personal bin
export PATH=~/bin:$PATH

# plan9
export PLAN9=/Users/hinmanm/src/plan9
export PATH=$PATH:$PLAN9/bin
export MANPATH=$MANPATH:$PLAN9/man

# go
export GOROOT=/usr/local/go
export GOPATH=~/.go
export PATH=$PATH:$GOPATH/bin

# node things
export PATH=$PATH:~/node_modules/.bin/

# crazy pip/homebrew things
export PATH=$PATH:/usr/local/share/python
export PYTHONSTARTUP=~/.pythonstartup

# haskell
export PATH=$PATH:~/Library/Haskell/bin

# Maven opts
export MAVEN_OPTS="-Xmx512m"

# manpath
export MANPATH=$MANPATH:/usr/local/man

# abbreviation
export EDITOR=emacs
export PAGER=less

# report things that take a while
export REPORTTIME=5

# CVS options
export CVSEDITOR=emacs

# RSPEC for autotest
export RSPEC=true

# ledger
export LEDGER_FILE=~/data/ledger.dat

# IRBRC for RVM
export IRBRC=~/.irbrc

# 10 second poll time for autossh
export AUTOSSH_POLL=10

# don't show load in prompt by default
export SHOW_LOAD=false

# start with a pre-title of nothing
export PRETITLE=""

# AWS stuff
export AWS_CONFIG_FILE=~/.notify-aws-creds
export AWS_DEFAULT_REGION=us-east-1
export QUEUENAME=''

# word chars
# default is: *?_-.[]~=/&;!#$%^(){}<>
export WORDCHARS="*?_-.[]~=&;!#$%^(){}<>\\"

# history
HISTFILE=$HOME/.zsh-history
HISTSIZE=10000
SAVEHIST=5000
# "persistent history"
# just write important commands you always need to ~/.important_commands
if [[ -r ~/.important_commands ]] ; then
    fc -R ~/.important_commands
fi

# support colors in less
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'



# zsh completion
if [ -d ~/.zsh/zsh-completions ] ; then
    fpath=(~/.zsh/zsh-completions/src $fpath)
fi
# additional completions
if [ -d ~/.zsh/completions ] ; then
    fpath=(~/.zsh/completions $fpath)
fi

zmodload zsh/complist
autoload -U compinit && compinit

zstyle ':completion:::::' completer _complete _approximate
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh-cache
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' hosts $ssh_hosts
zstyle ':completion:*:my-accounts' users-hosts $my_accounts
zstyle ':completion:*:other-accounts' users-hosts $other_accounts
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX + $#SUFFIX) / 3 )) )'
zstyle ':completion:*:descriptions' format "- %d -"
zstyle ':completion:*:corrections' format "- %d - (errors %e})"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
zstyle ':completion:*' verbose yes
zstyle ':completion:*' file-list list=20 insert=10


### OPTIONS ###
unsetopt bg_nice             # do NOT nice bg commands
unsetopt correct_all         # don't correct me, I know what I'm doing
setopt multios               # allow pipes to be split/duplicated
# ^^ try this: cat foo.clj > >(fgrep java | wc -l) > >(fgrep copy | wc -l)
setopt auto_cd
setopt extended_glob
setopt append_history
setopt extended_history
setopt share_history
setopt histignorealldups
setopt nohup
setopt longlistjobs
setopt notify
# I use dvorak, so correct spelling mistakes that a dvorak user would make
setopt dvorak
#setopt correct                # Spelling correction

# Nice renaming
autoload -U zmv
alias mmv='noglob zmv -W'

autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Keybindings
# Emacs keybindings
bindkey -e
bindkey "^?"    backward-delete-char
bindkey "^H"    backward-delete-char
bindkey "^[[3~" backward-delete-char
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line

bindkey '^r' history-incremental-search-backward
bindkey "^[[5~" up-line-or-history
bindkey "^[[6~" down-line-or-history
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
bindkey "^W" backward-delete-word
bindkey "^k" kill-line
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^I' complete-word # complete on tab, leave expansion to _expand
bindkey -r '^j' #unbind ctrl-j, I hit it all the time accidentaly
bindkey -r '^[x' # remove M-x for emacs-things


## GPG
# brew install gpg gpg-agent keychain
which keychain 2>&1 > /dev/null
if [[ $? -eq 0 ]]; then
    eval `keychain --eval --agents gpg,ssh --inherit any id_rsa`
fi


## Sourcing things

# Source z.sh if available
if [ -s ~/bin/z.sh ] ; then
    source ~/bin/z.sh ;
fi

# Use zsh syntax highlighting if available
if [ -s ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ] ; then
    source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# Use zsh-functional if available
if [ -s ~/.zsh/zsh-functional/functional.plugin.zsh ] ; then
    source ~/.zsh/zsh-functional/functional.plugin.zsh
fi

# Alias things
source ~/.zsh/aliases.zsh
# Functions
source ~/.zsh/functions.zsh
# Set prompt
source ~/.zsh/prompt.zsh
# ES helpers
source ~/.zsh/elasticsearch.zsh
# Sonian helpers
source ~/.zsh/sonian.zsh
# Linux commands
source ~/.zsh/linux.zsh
# OSX commands
source ~/.zsh/osx.zsh
# Setting tmux titles
source ~/.zsh/title.zsh
# Ruby-related stuff
source ~/.zsh/rvm.zsh

# run the startup commands
startup
