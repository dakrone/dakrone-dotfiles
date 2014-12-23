[[ -o interactive ]] && echo "+++Reading .zshenv"

OS=$(uname -s); export OS
MANPATH=/opt/local/man:/usr/local/man:$MANPATH
WORDCHARS='*?_[]~=&;!#$%^(){}'
# default is: *?_-.[]~=/&;!#$%^(){}<>
# other: "*?_-.[]~=&;!#$%^(){}<>\\"
WORDCHARS=${WORDCHARS:s,/,,}

export EDITOR=nano # to be overwritten later
export PAGER=less

export PATH=~/bin:/usr/local/bin:/usr/local/sbin:$PATH

# history
HISTFILE=$HOME/.zsh-history
HISTSIZE=10000
SAVEHIST=5000

export JAVA_HOME
[[ $OS == "Darwin" ]] && \
   JAVA_HOME=$(/usr/libexec/java_home -v 1.8)

## With Emacs 23, I've found this needs to go in ~root/.zshrc too to
## help with Tramp hangs.
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '
[[ ! $TERM == "dumb" ]] && TERM=xterm-256color
