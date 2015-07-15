#!/usr/bin/env zsh

#
#############
### WARNING
### This file has been automatically generated from an org-mode file
### Change at your own risk, as it may be overwritten later!
#############

# Things for dumb terminals
if [[ "$EMACSx" == "tx" || "$TERM" == "dumb" ]]; then
    unsetopt zle
    #unfunction precmd
    export DISABLE_AUTO_TITLE=true
    export ZSH_HIGHLIGHT_MAXLENGTH=0
else
    alias ag="ag --pager='less -FRX'"
fi
