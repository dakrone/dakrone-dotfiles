[[ -o interactive ]] && echo "+++Reading .zshenv"

OS=$(uname -s); export OS
MANPATH=/opt/local/man:/usr/local/man:$MANPATH
WORDCHARS='*?_[]~=&;!#$%^(){}'
WORDCHARS=${WORDCHARS:s,/,,}
LEDGER_FILE=$HOME/.ledger; export LEDGER_FILE
EDITOR=nano; export EDITOR

export JAVA_HOME
[[ $OS == "Darwin" ]] && \
   JAVA_HOME=$(/usr/libexec/java_home -v 1.7)

## With Emacs 23, I've found this needs to go in ~root/.zshrc too to
## help with Tramp hangs.
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '
[[ ! $TERM == "dumb" ]] && TERM=xterm-256color
