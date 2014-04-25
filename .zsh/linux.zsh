OS=$(uname)

## Emacs stuff
if [[ $OS == "Linux" ]]; then
    # make emacs have 256 colors
    alias emacs='TERM=xterm-256color emacs -nw'

    alias ec="emacsclient"

    function ek() { emacsclient -e '(kill-emacs)' -s $1 }

    alias e='emacs -nw'

    # no growl on linux, so back to regular
    alias lt='ltest'
    alias lrt='lein retest'

    export EDITOR="emacs -nw"

    alias tmux='tmux -2'

    # awesome
    alias gps='ps -eo cmd,fname,pid,pcpu,time --sort=-pcpu | head -n 11 && echo && ps -eo cmd,fname,pid,pmem,rss --sort=-rss | head -n 9'

    # java things
    # export JAVA_HOME=/home/hinmanm/jvm/current
    # export PATH=$PATH:$JAVA_HOME/bin
fi
