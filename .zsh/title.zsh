# Lifted from OMZ
function title {
    [ "$DISABLE_AUTO_TITLE" != "true" ] || return
    if [[ "$TERM" == screen* ]]; then
        #set screen hardstatus, usually truncated at 20 chars
        print -Pn "\ek$1:q\e\\"
    elif [[ "$TERM" == xterm* ]] || [[ $TERM == rxvt* ]] || [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
        #set window name
        print -Pn "\e]2;$2:q\a"
        #set icon (=tab) name (will override window name on broken terminal)
        print -Pn "\e]1;$1:q\a"
    fi
}

TERM_TAB_TITLE_IDLE="%15<..<%~%<<" #15 char left truncated PWD
TERM_TITLE_IDLE="%n@%m: %~"

#Appears when you have the prompt
function termsupport_precmd {
    title "$PRETITLE$TERM_TAB_TITLE_IDLE" $TERM_TITLE_IDLE
}

function notitle () { export DISABLE_AUTO_TITLE=true }
function settitle () { export DISABLE_AUTO_TITLE=false }

#Appears at the beginning of (and during) of command execution
function termsupport_preexec {
    emulate -L zsh
    setopt extended_glob
    #cmd name only, or if this is sudo or ssh, the next cmd
    local CMD=${1[(wr)^(*=*|sudo|ssh|-*)]}
    #echo "setting title to $CMD"
    title "$PRETITLE$CMD" "%100>...>$2%<<"
}

autoload -U add-zsh-hook
add-zsh-hook precmd  termsupport_precmd
add-zsh-hook preexec termsupport_preexec
