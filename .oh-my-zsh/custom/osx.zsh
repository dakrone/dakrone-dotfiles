OS=$(uname)

if [[ $OS == "Darwin" ]]; then

    export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home


    export EMACS_HOME="/Applications/Emacs.app/Contents/MacOS"

    if [ -s /usr/local/bin/emacs ]; then
        alias emacs='TERM=xterm-256color emacs'
        alias hb_emacs='/usr/local/bin/emacs'
    fi

    #function ec() { TERM=xterm-256color PATH=$EMACS_HOME/bin:$PATH emacsclient -t $@ }
    alias -g ec="TERM=xterm-256color PATH=$EMACS_HOME/bin:$PATH emacsclient -t"

    function es() { e --daemon=$1 && ec -s $1 }
    #function el() { ps ax|grep Emacs }
    function ek() { $EMACS_HOME/bin/emacsclient -e '(kill-emacs)' -s $1 }

    function tmacs () { TERM=xterm-256color /Applications/Emacs.app/Contents/MacOS/Emacs -nw }
    alias emacs=tmacs

    export EDITOR="TERM=xterm-256color /Applications/Emacs.app/Contents/MacOS/Emacs -nw"

    # Use MacVim's vim for terminal sessions, since it has everything compiled in.
    alias vim='/Applications/MacVim.app/Contents/MacOS/Vim'

    # Remove ctrl+y from the keybinds for delayed suspend
    stty dsusp undef

    # growled lein test
    alias lt='ltest'
    alias lrt='lrtest'
    # growled lein ptest (parallel test)
    alias lpt='lptest'

    # awesome
    alias gps="ps -c -r -ax -o command,pid,pcpu,time | sed 's/\(PID *\)%/\1 %/' | head -n 11 && echo && ps -c -m -ax -o command,pid,pmem,rss=RSIZE | sed 's/\(.\{23\}\)/\1 /' | head -n 9"

fi
