#!/usr/bin/env zsh

#
#############
### WARNING
### This file has been automatically generated from an org-mode file
### Change at your own risk, as it may be overwritten later!
#############

if [ -f ~/.globalrc ]; then
    export GTAGSCONF=$HOME/.globalrc
elif [ -f /usr/local/share/gtags/gtags.conf ] ; then
    export GTAGSCONF=/usr/local/share/gtags/gtags.conf
fi

export GTAGSLABEL=ctags
