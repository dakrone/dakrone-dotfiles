#!/usr/bin/env zsh

#
#############
### WARNING
### This file has been automatically generated from an org-mode file
### Change at your own risk, as it may be overwritten later!
#############

# functions
function history-all { history -E 1 }

# function to fix ssh agent
function fix-agent() {
    disable -a ls
    export SSH_AUTH_SOCK=`ls -t1 $(find /tmp/ -uid $UID -path \\*ssh\\* -type s 2> /dev/null) | head -1`
    enable -a ls
}

## TODO make these scripts instead of functions

# Check if a URL is up
function chk-url() {
    curl -sL -w "%{http_code} %{url_effective}\\n" "$1" -o /dev/null
}

# Tunnel ES from somewhere to here locally on port 9400
function es-tunnel() {
    autossh -M0 $1 -L 9400:localhost:9200 -CNf
}

# Tunnel logstash/kibana locally
function kibana-tunnel() {
    autossh -M0 $1 -L 9292:localhost:9292 -CNf
}

# Delete a branch locally and on my (dakrone) fork
function del-branch() {
    git branch -D $1
    git push dakrone :$1
}

# look up a process quickly
function pg {
    # doing it again afterwards for the coloration
    ps aux | fgrep -i $1 | fgrep -v fgrep | fgrep -i $1
}

# cd back up to the highest level git repo dir
# thanks Dan!
function cds () {
    ORIGINAL_PWD=`pwd`
    while [ ! -d ".git" -a `pwd` != "/" ]
    do
        cd ..
    done
    if [ ! -d ".git" ]
    then
        cd $ORIGINAL_PWD
    fi
}
