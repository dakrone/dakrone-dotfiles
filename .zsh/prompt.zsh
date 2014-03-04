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

load () {
    setopt localoptions noksharrays
    local -h loadavg

    case "${OSTYPE:l}" in
        linux*|cygwin*)
            local -h one five fifteen rest
            read one five fifteen rest </proc/loadavg
            loadavg="$one $five $fifteen"
            ;;
        openbsd*)
            loadavg=$(uptime | sed -e 's/.*load averages: \([.[:digit:]]*\), \([.[:digit:]]*\), \([.[:digit:]]\)/\1 \2 \3/')
            ;;
        darwin*)
            loadavg=$(uptime | sed -e 's/.*load averages: \([.[:digit:]]*\) \([.[:digit:]]*\) \([.[:digit:]]\)/\1 \2 \3/')
            ;;
    esac
    print $loadavg
}

#######################################
# based on http://eseth.org/2009/nethack-term.html#post-nethack-term
# and http://eseth.org/2010/git-in-zsh.html

# Define some random pets to wander around
PROMPT_PETS=("λ" "✝" "ƒ" "π" "♮" "§" "☊" "☋" "☩")
# Randomly choose one for the session
export PROMPT_PET=$PROMPT_PETS[$RANDOM%$#PROMPT_PETS+1]

function setprompt() {
    local -a lines infoline
    local x i pet dungeon filler i_width i_pad

    # how about a pet lambda? :)
    #pet=λ
    pet=$PROMPT_PET

    ### First, assemble the top line
    # Current dir; show in yellow if not writable
    [[ -w $PWD ]] && infoline+=( ${blue} ) || infoline+=( ${yellow} )
    infoline+=( "%~${reset}" )

    # Username & host
    infoline+=( "${green}%n${gray}" )
    [[ -n $SSH_CLIENT ]] && infoline+=( "${gray}@${green}%m${gray}" )
    infoline+=( " ‹⋅⋅" )

    # Strip color to find text width & make the full-width filler
    zstyle -T ":pr-nethack:" show-pet && i_pad=4 || i_pad=0

    i_width=${(S)infoline//\%\{*\%\}} # search-and-replace color escapes
    i_width=${#${(%)i_width}} # expand all escapes and count the chars

    # the "-1" is for the < and > at each end
    filler="${gray}‹${(l:$(( $COLUMNS - $i_width - $i_pad - 1))::⋅:)}›${reset}"
    infoline[2]=( "${infoline[2]} ${filler} " )


    ### Now, assemble all prompt lines
    lines+=( ${(j::)infoline} )
    # vcs info
    [[ -n ${vcs_info_msg_0_} ]] && lines+=( "${gray}${vcs_info_msg_0_}${reset}" )
    # system load
    [[ "$SHOW_LOAD" == "true" ]] && lines+=( "‹$({load})›" )
    # input prompt
    lines+=( "%(1j.${gray}%j${reset} .)%(0?.${green2}.${red})∴${reset} " )

    ### Add dungeon floor to each line
    # Allow easy toggling of pet display
    if zstyle -T ":pr-nethack:" show-pet ; then
        dungeon=${(l:$(( ${#lines} * 3 ))::⋅:)}
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
       export DISABLE_AUTO_TITLE=true
       export ZSH_HIGHLIGHT_MAXLENGTH=0
    fi
}

function colorSetup {
    # ls colors
    autoload colors; colors;
    #export LS_COLORS

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

    # Setup the prompt with pretty colors
    setopt prompt_subst
}

# Initialize colors for setprompt2
colorSetup

function setprompt2 {
    if [ $UID -eq 0 ]; then NCOLOR="red"; else NCOLOR="green"; fi
    local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"

    PROMPT='$FG[237]⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅%{$reset_color%}
$FG[032]%~ ${gray}${vcs_info_msg_0_}
$FG[105]%(!.#.»)%{$reset_color%} '
    PROMPT2='%{$fg[red]%}\ %{$reset_color%}'
    RPS1='${return_code}'

    # color vars
    eval my_gray='$FG[237]'

    # right prompt
    # RPROMPT='$my_gray%n@%m%{$reset_color%}%'

    if [[ "$TERM" == "dumb" ]] then
       unsetopt zle
       unsetopt prompt_cr
       unsetopt prompt_subst
       unfunction precmd
       unfunction preexec
       PROMPT='‹ %~ › %(?..%? )∴ '
       export DISABLE_AUTO_TITLE=true
       export ZSH_HIGHLIGHT_MAXLENGTH=0
    fi
}

function precmd {
    if [ -s ~/bin/z.sh ] ; then
        z --add "$(pwd -P)"
    fi
    vcs_info
    setprompt2
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
