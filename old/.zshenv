#!/usr/bin/env zsh

#
#############
### WARNING
### This file has been automatically generated from an org-mode file
### Change at your own risk, as it may be overwritten later!
#############

[[ -o interactive ]] && echo "+++Reading .zshenv"

MANPATH=/opt/local/man:/usr/local/man:$MANPATH
WORDCHARS='*?_[]~=&;!#$%^(){}'
# default is: *?_-.[]~=/&;!#$%^(){}<>
# other: "*?_-.[]~=&;!#$%^(){}<>\\"
WORDCHARS=${WORDCHARS:s,/,,}
LEDGER_FILE=$HOME/ledger.dat; export LEDGER_FILE

export EDITOR=nano # to be overwritten later
export PAGER=less

# Update path with local ~/bin and cabal's bin dir
export PATH=~/bin:~/.cabal/bin:/usr/local/bin:/usr/local/sbin:$PATH

# Node/npm
export PATH=$PATH:~/node_modules/.bin

# history
HISTFILE=$HOME/.zsh-history
HISTSIZE=10000
SAVEHIST=5000

## Sourcing OS-specific things
OS=$(uname -s); export OS
if [[ -f ~/.zsh.d/zsh.${OS} ]]; then
    if [[ ! -z $ZSHDEBUG ]]; then
        echo +++ ~/.zsh.d/zsh.${OS}
    fi
    source ~/.zsh.d/zsh.${OS}
fi

## Sourcing machine-specific things
export HOSTPREFIX=`hostname | cut -d. -f1 | sed 's/.*/\L&/'`
if [[ -f ~/.zsh.d/zsh.${HOSTPREFIX} ]]; then
    if [[ ! -z $ZSHDEBUG ]]; then
        echo +++ ~/.zsh.d/zsh.${HOSTPREFIX}
    fi
    source ~/.zsh.d/zsh.${HOSTPREFIX}
fi

## With Emacs 23, I've found this needs to go in ~root/.zshrc too to
## help with Tramp hangs.
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '
[[ ! $TERM == "dumb" ]] && TERM=xterm-256color
