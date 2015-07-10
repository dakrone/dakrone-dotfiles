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

OS=$(uname -s); export OS

export JAVA_HOME
[[ $OS == "Darwin" ]] && \
   JAVA_HOME=$(/usr/libexec/java_home -v 1.8)

## With Emacs 23, I've found this needs to go in ~root/.zshrc too to
## help with Tramp hangs.
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '
[[ ! $TERM == "dumb" ]] && TERM=xterm-256color
