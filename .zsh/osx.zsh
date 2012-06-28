OS=$(uname)

if [[ $OS == "Darwin" ]]; then

    #export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home
    #export JAVA_HOME=/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home


    export EMACS_HOME="/Applications/Emacs.app/Contents/MacOS"

    if [ -s /usr/local/bin/emacs ]; then
        alias emacs='TERM=xterm-256color emacs'
        alias hb_emacs='/usr/local/bin/emacs'
    fi

    #function ec() { TERM=xterm-256color PATH=$EMACS_HOME/bin:$PATH emacsclient -t $@ }
    alias e="TERM=xterm-256color PATH=$EMACS_HOME/bin:$PATH $EMACS_HOME/Emacs -nw"
    alias ec="TERM=xterm-256color PATH=$EMACS_HOME/bin:$PATH emacsclient -t"

    #function el() { ps ax|grep Emacs }
    function ek() { $EMACS_HOME/bin/emacsclient -e '(kill-emacs)' -s $1 }

    # function tmacs () {
    #     TERM=xterm-256color /Applications/Emacs.app/Contents/MacOS/Emacs -nw $@
    # }

    alias emacs="TERM=xterm-256color PATH=$EMACS_HOME/bin:$PATH $EMACS_HOME/Emacs -nw"

    export EDITOR="TERM=xterm-256color PATH=$EMACS_HOME/bin:$PATH $EMACS_HOME/Emacs -nw"

    # Use MacVim's vim for terminal sessions, since it has everything compiled in.
    alias vim='/Applications/MacVim.app/Contents/MacOS/Vim'

    # Remove ctrl+y from the keybinds for delayed suspend
    stty dsusp undef

    # growled lein test
    alias lt='ltest'
    alias lrt='lrtest'
    alias l2t='l2test'
    alias l2rt='l2rtest'

    # awesome
    alias gps="ps -c -r -ax -o command,pid,pcpu,time | sed 's/\(PID *\)%/\1 %/' | head -n 11 && echo && ps -c -m -ax -o command,pid,pmem,rss=RSIZE | sed 's/\(.\{23\}\)/\1 /' | head -n 9"

fi
