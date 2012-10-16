OS=$(uname)

# path
export PATH=/usr/local/bin:$PATH
export PATH=$PATH:/usr/local/sbin:/usr/libexec:/opt/local/sbin
export PATH=$PATH:/usr/local/mysql/bin:~/.cabal/bin
export PATH=$PATH:/usr/local/git/libexec/git-core
# Always override with my personal bin
export PATH=~/bin:$PATH

# plan9
export PLAN9=/Users/hinmanm/src/plan9
export PATH=$PATH:$PLAN9/bin
export MANPATH=$MANPATH:$PLAN9/man

# go
export GOROOT=/usr/local/go
export PATH=$PATH:$GOROOT/bin

# Java opts (leiningen uses these)
#export JAVA_OPTS="-Dfile.encoding=UTF-8 -Dslime.encoding=UTF-8 -Xmx512m -XX:+HeapDumpOnOutOfMemoryError"

# manpath
export MANPATH=$MANPATH:/usr/local/man

# abbreviation
export EDITOR=emacs
export PAGER=less

# report things that take a while
export REPORTTIME=5

# node things
export NODE_PATH=/usr/local/lib/node:/usr/local/lib/node_modules
export PATH=$PATH:/usr/local/share/npm/bin

# CVS options
export CVSEDITOR=emacs

# RSPEC for autotest
export RSPEC=true

# ledger
export LEDGER_FILE=~/data/ledger.dat

# IRBRC for RVM
export IRBRC=~/.irbrc

# 20 second poll time for autossh
export AUTOSSH_POLL=20

# don't show load in prompt by default
export SHOW_LOAD=false

# start with a pre-title of nothing
export PRETITLE=""

# drip
DRIP_SHUTDOWN=30

# word chars
# default is: *?_-.[]~=/&;!#$%^(){}<>
export WORDCHARS="*?_-.[]~=/&;!#$%^(){}<>\"'\\"

export JIRAPATH=~/src/jira
alias jira="$JIRAPATH/jira.sh --action getIssue --issue "

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
    fpath=(~/.zsh/zsh-completions $fpath)
fi

zmodload zsh/complist
autoload -U compinit && compinit

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' hosts $ssh_hosts
zstyle ':completion:*:my-accounts' users-hosts $my_accounts
zstyle ':completion:*:other-accounts' users-hosts $other_accounts
zstyle ':completion:::::' completer _complete _approximate
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX + $#SUFFIX) / 3 )) )'
zstyle ':completion:*:descriptions' format "- %d -"
zstyle ':completion:*:corrections' format "- %d - (errors %e})"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
zstyle ':completion:*' verbose yes


### OPTIONS ###
unsetopt bg_nice             # do NOT nice bg commands
unsetopt correct_all         # don't correct me, I know what I'm doing
setopt multios               # allow pipes to be split/duplicated
setopt auto_cd
setopt extended_glob
setopt append_history
setopt extended_history
setopt share_history
setopt histignorealldups
setopt nohup
setopt longlistjobs
setopt notify
# ^^ try this: cat foo.clj > >(fgrep java | wc -l) > >(fgrep copy | wc -l)

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



## GPG
# brew install gpg gpg-agent keychain
which keychain 2>&1 > /dev/null
if [[ $? -eq 0 ]]; then
    eval `keychain --eval --agents gpg,ssh --inherit any id_rawpacket`
fi


## Sourcing things

# Source z.sh if available
if [ -s ~/bin/z.sh ] ; then
    source ~/bin/z.sh ;
fi

# rvm stuff (if it exists):
# if [ -s ~/.rvm/scripts/rvm ] ; then
#     source ~/.rvm/scripts/rvm
#     # Set default ruby install
#     rvm default
# fi

# rbenv stuff
if [ -s ~/.rbenv ]; then
    eval "$(rbenv init -)"
fi

# Use zsh syntax highlighting if available
if [ -s ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ] ; then
    source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
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
# Misc funcitions
source ~/.zsh/misc.zsh

# run the startup commands
startup
