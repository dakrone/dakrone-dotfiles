# Things for dumb terminals
if [[ "$EMACSx" == "tx" || "$TERM" == "dumb" ]]; then
    unsetopt zle
    #unfunction precmd
    export DISABLE_AUTO_TITLE=true
    export ZSH_HIGHLIGHT_MAXLENGTH=0
else
    alias ag="ag --pager='less -FRX'"
fi
