#!/usr/bin/env zsh

#
#############
### WARNING
### This file has been automatically generated from an org-mode file
### Change at your own risk, as it may be overwritten later!
#############

autoload colors
colors

git_branch() {
    git branch --no-color 2>/dev/null | grep '^*' | colrm 1 2
    # $pipestatus[1] for the git exit code
}

autoload -Uz vcs_info

if [[ ! $TERM = "dumb" ]]; then
    zstyle ":vcs_info:*" check-for-changes true
    zstyle ":vcs_info:*" stagedstr "%F{green}*"
    zstyle ":vcs_info:*" unstagedstr "%F{yellow}*"
    zstyle ":vcs_info:(sv[nk]|bzr):*" branchformat "%b%F{1}:%F{yellow}%r%{$reset_color%}"
    zstyle ":vcs_info:*" enable git svn bzr hg
    precmd () {
        if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
               zstyle ":vcs_info:*" formats "%b%c%u%{$reset_color%}"
           } else {
               zstyle ":vcs_info:*" formats "%b%c%u%F{red}*%{$reset_color%}"
           }
           vcs_info
    }
else
    zstyle ":vcs_info:*" check-for-changes true
    zstyle ":vcs_info:*" stagedstr "*"
    zstyle ":vcs_info:*" unstagedstr "*"
    zstyle ":vcs_info:(sv[nk]|bzr):*" branchformat "%b:%r"
    zstyle ":vcs_info:*" enable git svn bzr hg
    precmd () {
        if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
               zstyle ":vcs_info:*" formats "%b%c%u"
           } else {
               zstyle ":vcs_info:*" formats "%b%c%u*"
           }
           vcs_info
    }
fi
