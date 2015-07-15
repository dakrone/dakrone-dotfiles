#!/usr/bin/env zsh

#
#############
### WARNING
### This file has been automatically generated from an org-mode file
### Change at your own risk, as it may be overwritten later!
#############

autoload -U add-zsh-hook
autoload -U colors && colors
autoload -Uz vcs_info
setopt prompt_subst

local gray="%{$fg_bold[black]%}"
local green="%{$fg_bold[green]%}"
local blue="%{$fg[blue]%}"
local red="%{$fg[red]%}"
local yellow="%{$fg[yellow]%}"

zstyle ':vcs_info:*' enable git svn cvs hg
zstyle ':vcs_info:git*:*' get-revision true
zstyle ':vcs_info:git*:*' check-for-changes true

# hash changes branch misc
zstyle ':vcs_info:git*' formats "(%s) %8.8i ${green}%c${red}%u${gray} %b%m"
zstyle ':vcs_info:git*' actionformats "(%s|${yellow}%a${gray}) %8.8i ${green}%c${red}%u${gray} %b%m"
zstyle ':vcs_info:git*+set-message:*' hooks git-st git-stash

# Show remote ref name and number of commits ahead-of or behind
function +vi-git-st() {
    local ahead behind remote
    local -a gitstatus

    # Are we on a remote-tracking branch?
    remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} \
                   --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

    if [[ -n ${remote} ]] ; then
        ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l | tr -d " ")
        (( $ahead )) && gitstatus+=( "${green}+${ahead}${gray}" )

        behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l | tr -d " ")
        (( $behind )) && gitstatus+=( "${red}-${behind}${gray}" )

        if [[ -n ${gitstatus} ]] ; then
            hook_com[branch]="${hook_com[branch]} [${remote} ${(j:/:)gitstatus}]"
        else
            hook_com[branch]="${hook_com[branch]} [${remote}]"
        fi
    fi
}

# Show count of stashed changes
function +vi-git-stash() {
    local -a stashes
    if [[ -s ${hook_com[base]}/.git/refs/stash ]] ; then
        stashes=$(git stash list 2>/dev/null | wc -l | tr -d " ")
        hook_com[misc]+=" (${stashes} stashed)"
    fi
}

function colorSetup {
    # A script to make using 256 colors in zsh less painful.
    # P.C. Shyamshankar <sykora@lucentbeing.com>
    # Copied from http://github.com/sykora/etc/blob/master/zsh/functions/spectrum/

    typeset -Ag FX FG BG

    FX=(
        reset     "%{[00m%}"
        bold      "%{[01m%}" no-bold      "%{[22m%}"
        italic    "%{[03m%}" no-italic    "%{[23m%}"
        underline "%{[04m%}" no-underline "%{[24m%}"
        blink     "%{[05m%}" no-blink     "%{[25m%}"
        reverse   "%{[07m%}" no-reverse   "%{[27m%}"
    )

    for color in {000..255}; do
        FG[$color]="%{[38;5;${color}m%}"
        BG[$color]="%{[48;5;${color}m%}"
    done

    # Show all 256 colors with color number
    function spectrum_ls() {
        for code in {000..255}; do
            print -P -- "$code: %F{$code}Test%f"
        done
    }

    # Show all 256 colors where the background is set to specific color
    function spectrum_bls() {
        for code in {000..255}; do
            ((cc = code + 1))
            print -P -- "$BG[$code]$code: Test %{$reset_color%}"
        done
    }
}

# Initialize colors for setprompt2
colorSetup

# old-prompt
PROMPT='$FG[032]%~ $FG[237]${vcs_info_msg_0_}
$FG[105]%(?..${red}%?$FG[105] )%(!.#.»)%{$reset_color%} '

add-zsh-hook precmd vcs_info

# Simple prompt setup
# if not_in_cloud; then
#     # PROMPT='%n@%m %w %* %! %? %B%3~%b(${vcs_info_msg_0_})%# '; export PROMPT
#     PROMPT='%n@%m %? %B%3~%b(${vcs_info_msg_0_})%# '; export PROMPT
# else
#     PROMPT='%n@%m %? %~%# '; export PROMPT
# fi
