#!/usr/bin/env zsh

#
#############
### WARNING
### This file has been automatically generated from an org-mode file
### Change at your own risk, as it may be overwritten later!
#############

# colorful ls for whichever platform
if ls -F --color=auto >&/dev/null; then
    alias ls="ls --color=auto -F"
else
    alias ls="ls -GF"
fi
# various ls helpers
alias l.='ls -d .*'
alias ll='ls -lh'
alias l='ls -lh'
alias la='ls -alh'
alias lr='ls -lR'
# colorize greps
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
# make less a little more sane
alias less='less -RX'
# various port forwarding and hole-punching
alias scsetup='sudo socat -d -d TCP4-listen:6666,fork OPENSSL:typoet.com:443,cert=host.pem,verify=0'
alias scsetup2='sudo socat -d -d TCP4-listen:7777,fork OPENSSL:blackex:443,cert=host.pem,verify=0'
# reverse proxy & keepopen
alias prox='ssh -nNT -R 4444:localhost:22 writequit.org'
alias autoprox='autossh -M 22000 -nNT -R 4444:localhost:22 writequit.org'
# open elinks quickly
alias el='TERM=xterm-color elinks'
# start a master tmux
alias tmaster='tmux -2 -u -S /tmp/mastermux -f .tmux.master'

# datetime aliases
alias dt='gdate "+%Y-%m-%dT%H:%M:%S.%3N%zZ"'
# Elasticsearch's basic_date_time
alias bdt='gdate "+%Y%m%dT%H%M%S.%3N%z"'
alias epoch='date +%s'
# jump start to magit
alias magit='emacs -f magit-status'
# simple-extract
alias se="tar zxvf"
alias ga="git annex"
# download manager
alias aria2c='aria2c -c -x5 -s10 -m0'
# sync org files
alias org2ivalice='rsync -azP --delete ~/org/ ivalice-local:~/org'
alias ivalice2org='rsync -azP --delete ivalice-local:~/org/ ~/org'
alias xanadu2org='rsync -azP --delete xanadu:~/org/ ~/org'
alias org2xanadu='rsync -azP --delete ~/org/ xanadu:~/org'
