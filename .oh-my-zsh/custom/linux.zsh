OS=$(uname)

## Emacs stuff
if [[ $OS == "Linux" ]]; then
    # make emacs have 256 colors
    alias emacs='TERM=xterm-256color emacs'

    function ec() { TERM=xterm-256color emacsclient -t $@ }
    # no growl on linux, so back to regular
    alias lt='lein test'
    alias lrt='lein retest'

    export EDITOR="emacs"

    # awesome
    alias gps='ps -eo cmd,fname,pid,pcpu,time --sort=-pcpu | head -n 11 && echo && ps -eo cmd,fname,pid,pmem,rss --sort=-rss | head -n 9'

    #lotus tests
    export LD_LIBRARY_PATH=/opt/ibm/lotus/notes/
fi

