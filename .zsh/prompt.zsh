autoload -U colors && colors
autoload -Uz vcs_info
setopt prompt_subst
local reset white gray green green2 blue red yellow

reset="%{${reset_color}%}"
white="%{$fg[white]%}"
gray="%{$fg_bold[black]%}"
green="%{$fg_bold[green]%}"
green2="%{$fg[green]%}"
blue="%{$fg[blue]%}"
red="%{$fg[red]%}"
yellow="%{$fg[yellow]%}"

zstyle ':vcs_info:*' enable git svn cvs hg bzr darcs
zstyle ':vcs_info:git*:*' get-revision true
zstyle ':vcs_info:git*:*' check-for-changes true

# hash changes branch misc
zstyle ':vcs_info:git*' formats "(%s) %12.12i ${green}%c${red}%u${gray} %b%m"
zstyle ':vcs_info:git*' actionformats "(%s|%a) %12.12i %c%u %b%m"
zstyle ':vcs_info:git*+set-message:*' hooks git-st git-stash

# Show remote ref name and number of commits ahead-of or behind
function +vi-git-st() {
    local ahead behind remote
    local -a gitstatus

    # Are we on a remote-tracking branch?
    remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} \
        --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

    if [[ -n ${remote} ]] ; then
        # for git prior to 1.7
        # ahead=$(git rev-list origin/${hook_com[branch]}..HEAD | wc -l)
        ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l | tr -d " ")
        (( $ahead )) && gitstatus+=( "${green}+${ahead}${gray}" )

        # for git prior to 1.7
        # behind=$(git rev-list HEAD..origin/${hook_com[branch]} | wc -l)
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

#######################################
# based on http://eseth.org/2009/nethack-term.html#post-nethack-term
# and http://eseth.org/2010/git-in-zsh.html
function setprompt() {
    local -a lines infoline
    local x i pet dungeon filler i_width i_pad

    # A domestic animal, the _tame dog_ (_Canis familiaris_)
    #pet=d
    # how about a pet lambda instead? :)
    pet=λ

    ### First, assemble the top line
    # Current dir; show in yellow if not writable
    [[ -w $PWD ]] && infoline+=( ${blue} ) || infoline+=( ${yellow} )
    infoline+=( "%~${reset}" )

    # Username & host
    infoline+=( "${green}%n${gray}" )
    [[ -n $SSH_CLIENT ]] && infoline+=( "${gray}@${green}%m${gray}" )

    # Strip color to find text width & make the full-width filler
    zstyle -T ":pr-nethack:" show-pet && i_pad=4 || i_pad=0

    i_width=${(S)infoline//\%\{*\%\}} # search-and-replace color escapes
    i_width=${#${(%)i_width}} # expand all escapes and count the chars

    filler="${gray}‹${(l:$(( $COLUMNS - $i_width - $i_pad - 2))::·:)}›${reset}"
    infoline[2]=( "${infoline[2]} ${filler} " )

    ### Now, assemble all prompt lines
    lines+=( ${(j::)infoline} )
    [[ -n ${vcs_info_msg_0_} ]] && lines+=( "${gray}${vcs_info_msg_0_}${reset}" )
    lines+=( "%(1j.${gray}%j${reset} .)%(0?.${green2}.${red})∴${reset} " )

    ### Add dungeon floor to each line
    # Allow easy toggling of pet display
    if zstyle -T ":pr-nethack:" show-pet ; then
        dungeon=${(l:$(( ${#lines} * 3 ))::.:)}
        dungeon[$[${RANDOM}%${#dungeon}]+1]=$pet

        for (( i=1; i < $(( ${#lines} + 1 )); i++ )) ; do
            case $i in
                1) x=1;; 2) x=4;; 3) x=7;; 4) x=10;;
            esac
            lines[$i]="${gray}${dungeon[x,$(( $x + 2 ))]} ${lines[$i]}${reset}"
        done
    fi

    ### Finally, set the prompt
    PROMPT=${(F)lines}
    if [[ "$TERM" == "dumb" ]] then
        unsetopt zle
        unsetopt prompt_cr
        unsetopt prompt_subst
        unfunction precmd
        unfunction preexec
        PROMPT='‹ %~ › %(?..%? )∴ '
    fi
}

function precmd {
    if [ -s ~/bin/z.sh ] ; then
        z --add "$(pwd -P)"
    fi
    vcs_info
    setprompt
}

# Disable stuff when your term sucks
function simple() {
    export DISABLE_AUTO_TITLE=true
    export ZSH_HIGHLIGHT_MAXLENGTH=0
}

# Re-enable the pretty things
function complex() {
    export DISABLE_AUTO_TITLE=false
    unset ZSH_HIGHLIGHT_MAXLENGTH
}
