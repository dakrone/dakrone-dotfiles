OS=$(uname)

# autoloads
autoload -U compinit
autoload -U promptinit
autoload colors
colors
compinit
promptinit

# path
export PATH=/opt/local/bin:/usr/local/bin:$PATH:/usr/local/sbin:/usr/local/sbin:/usr/libexec:/opt/local/sbin:/usr/local/mysql/bin
# Path for Matasano's blackbag
export PATH=/usr/local/bin/blackbag:$PATH
# Path for ruby gems
export PATH=$PATH:/var/lib/gems/1.8/bin
# Path for postgres
export PATH=$PATH:/opt/local/lib/postgresql84/bin
# Path for local gems
export PATH=$PATH:~/.gem/ruby/1.8/bin
export PATH=$PATH:~/.gem/ruby/1.9/bin
# Path for git
#export PATH=$PATH:/usr/local/git/bin
# Path for liebke's cljr - http://github.com/liebke/cljr
#export PATH=/Users/hinmanm/.cljr/bin:$PATH
# path for maven
export PATH=$PATH:/opt/apache-maven-2.2.1

# Chris' ruby stuff
export RUBYLIB=~/src/chrisbin/ruby
export RUBYOPT=rubygems
export PATH=$PATH:~/src/chrisbin:~/src/chrisbin/ruby

# I'm using java 1.6 on OSX
export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home

# VimClojure stuff (for nailgun server)
export CLOJURE_EXT=/Users/hinmanm/.vimclojure:/Users/hinmanm/.cljr/lib:./lib:./classes:./src:.
export CLOJURE_OPTS="-server -Dfile.encoding=UTF-8 -Dslime.encoding=UTF-8 -Xmx512 -XX:+HeapDumpOnOutOfMemoryError"

# Java opts (leiningen uses these)
export JAVA_OPTS="-server -Dfile.encoding=UTF-8 -Dslime.encoding=UTF-8 -Xmx512m -XX:+HeapDumpOnOutOfMemoryError"

# manpath
export MANPATH=$MANPATH:/usr/local/man:/opt/local/share/man

# abbreviation
export EDITOR=vim
export PAGER=less

export OPSCODE_USER="sonian_developer"
export ENV="dev"

# CVS for HeX
#export CVSROOT=:ext:dakrone@cvsup.rawpacket.org:/home/project/rawpacket/cvs

# CVS options
export CVSEDITOR=vim

# RSPEC for autotest
export RSPEC=true

# Source j.sh
if [ -s ~/bin/j.sh ] ; then source ~/bin/j.sh ; fi

# rvm stuff:
if [ -s ~/.rvm/scripts/rvm ] ; then
      source ~/.rvm/scripts/rvm
      # Set default ruby install
      rvm default
fi

# Always override with my personal bin
export PATH=~/bin:$PATH

# IRBRC for RVM
export IRBRC=~/.irbrc

# Term settings, if we exist as a screen term, use xterm-color instead of screen-bce.
# Otherwise, leave the TERM var alone, because we need it to set terminal titles correctly
case $TERM in
      screen*)
            export TERM=xterm-color
      ;;
esac

# set aliases
alias mv='nocorrect mv'       # no spelling correction on mv
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'
#alias j=jobs
if ls -F --color=auto >&/dev/null; then
  alias ls="ls --color=auto -F"
else
  alias ls="ls -GF"
fi
alias l.='ls -d .*'
alias ll='ls -lh'
alias la='ls -alh'
alias lr='ls -lR'
alias less='less -FRX'
alias grep='grep -i --color=auto'
alias egrep='egrep -i --color=auto'
alias cd..='cd ..'
alias ..='cd ..'
alias nsmc='cd ~/src/ruby/nsm-console'
alias serv='cat /etc/services | grep'
alias pg='ps aux | grep'
alias nl='sudo netstat -tunapl'
alias dmesg='sudo dmesg'
alias remhex='ssh -i ~/.ssh/id_rawpacket dakrone@localhost -p 6666'
alias remblack='ssh -i ~/.ssh/id_rawpacket hinmanm@localhost -p 7777'
alias remstyx='ssh -i ~/.ssh/id_rawpacket lee@localhost -p 6666'
alias scsetup='sudo socat -d -d TCP4-listen:6666,fork OPENSSL:typoet.com:443,cert=host.pem,verify=0'
alias scsetup2='sudo socat -d -d TCP4-listen:7777,fork OPENSSL:blackex:443,cert=host.pem,verify=0'
alias blackexprox='ssh -i ~/.ssh/id_rawpacket -ND 9999 hinmanm@localhost -p 7777'
alias blackprox='ssh -i ~/.ssh/id_rawpacket -ND 9999 hinmanm@black'
alias styxprox='ssh -i ~/.ssh/id_rawpacket -ND 9999 lee@localhost -p 6666'
alias xprox='ssh -nNT -R 3333:localhost:2222 x'
alias tcpdump='tcpdump -ttttnnn'
alias vless=/usr/share/vim/vim72/macros/less.sh
alias week='remind -c+1 ~/.reminders'
alias month='remind -c ~/.reminders'
alias gps='geektool-ps'
alias jl='j --l'
alias jr='j --r'
alias js='j --s'
alias givm='gvim'
# Colored rspec
alias cspec='spec -c --format specdoc'
# Tmux stuff
# force 256 color mode
alias tmux='tmux -2'
alias rvim='gvim --remote-tab-silent'

if [[ $OS == "Darwin" ]]; then
    EMACS_HOME="/Applications/Emacs.app/Contents/MacOS"

    function e()  { PATH=$EMACS_HOME/bin:$PATH $EMACS_HOME/Emacs $@ }
    function ec() { PATH=$EMACS_HOME/bin:$PATH emacsclient -t $@ }

    function es() { e --daemon=$1 && ec -s $1 }
    function el() { ps ax|grep Emacs }
    function ek() { $EMACS_HOME/bin/emacsclient -e '(kill-emacs)' -s $1 }

    alias emacs=e

#    export EDITOR="ec -c"
#    export ALTERNATIVE_EDITOR="e"

    alias screen='TERM=xterm-color && /opt/local/bin/screen'
# Use MacVim's vim for terminal sessions, since it has everything compiled in.
    alias vim='/Applications/MacVim.app/Contents/MacOS/Vim'
    alias hb_emacs='/usr/local/bin/emacs'
    # alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
fi

alias todo='ec -n ~/work.org'


# history
HISTFILE=$HOME/.zsh-history
HISTSIZE=5000
SAVEHIST=1000
setopt appendhistory autocd extendedglob
setopt share_history
function history-all { history -E 1 }


# functions
mdc() { mkdir -p "$1" && cd "$1" }
setenv() { export $1=$2 }  # csh compatibility
sdate() { date +%Y.%m.%d }
rot13 () { tr "[a-m][n-z][A-M][N-Z]" "[n-z][a-m][N-Z][A-M]" }
function maxhead() { head -n `echo $LINES - 5|bc` ; }
function maxtail() { tail -n `echo $LINES - 5|bc` ; }
function git_dirty_flag() {
  git status 2> /dev/null | grep -c : | awk '{if ($1 > 0) print "⚡"}'
}
function parse_git_branch() {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

get-git-branch() {
      ref=$(git symbolic-ref HEAD 2>/dev/null | cut -d'/' -f3,4,5)
      echo $ref
}


### Prompt ###
setopt prompt_subst
export PS1="%{$fg[grey]%}‹ %{$fg[blue]%}%~%{$fg[grey]%} %{$fg[green]%}$(get-git-branch)%{$fg[grey]%}› %{$fg[green]%}∴%{$reset_color%} "
autoload -U promptinit
promptinit

export RPROMPT="%{$reset_color%}"

# format titles for screen and rxvt
function title() {
  # escape '%' chars in $1, make nonprintables visible
  a=${(V)1//\%/\%\%}

  # Truncate command, and join lines.
  a=$(print -Pn "%40>...>$a" | tr -d "\n")

  case $TERM in
  screen*)
    print -Pn "\ek$a:$3\e\\"      # screen title (in ^A")
    ;;
  xterm-color*)
    print -Pn "\ek$a:$3\e\\"      # screen title (in ^A")
    ;;
  xterm*|rxvt)
    print -Pn "\e]2;$2 | $a:$3\a" # plain xterm title
    ;;
  esac
}

# precmd is called just before the prompt is printed
function precmd() {
  title "zsh" "$USER@%m" "%55<...<%~"

  # Set the git branch in prompt
  export PS1="%{$fg[grey]%}‹ %{$fg[blue]%}%~%{$fg[grey]%} %{$fg[green]%}$(get-git-branch)%{$fg[grey]%}› %{$fg[green]%}∴%{$reset_color%} "
}

# preexec is called just before any command line is executed
function preexec() {
  title "$1" "$USER@%m" "%35<...<%~"
}

# Make w/growl support
function gmake () {
  DIR=`pwd`
  make $1 $2 $3 $4 $5 $6 $7 $8 $9
  if [[ $? == 0 ]]; then
    growlnotify -m "'make $1' successful in $DIR" 
  else
    growlnotify -m "'make $1' failed in $DIR" 
  fi
}

# Configure w/growl support
function gconf () {
  DIR=`pwd`
  ./configure $1 $2 $3 $4 $5 $6 $7 $8 $9 
  if [[ $? == 0 ]]; then
    growlnotify -m "'configure' successful in $DIR" 
  else
    growlnotify -m "'configure' failed in $DIR" 
  fi
}

# colorful listings
zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' hosts $ssh_hosts
zstyle ':completion:*:my-accounts' users-hosts $my_accounts
zstyle ':completion:*:other-accounts' users-hosts $other_accounts

### OPTIONS ###
unsetopt BG_NICE		      # do NOT nice bg commands
setopt EXTENDED_HISTORY		# puts timestamps in the history
setopt NO_HUP                 # don't send kill to background jobs when exiting

# Keybindings
bindkey -e
bindkey "^?"    backward-delete-char
bindkey "^H"    backward-delete-char
bindkey "^[[3~" backward-delete-char
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line

bindkey '^r' history-incremental-search-backward
bindkey "^[[5~" up-line-or-history
bindkey "^[[6~" down-line-or-history
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
bindkey "^W" backward-delete-word
#bindkey "^b" backward-word
#bindkey "^f" forward-word
bindkey "^d" delete-word
bindkey "^k" kill-line
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^I' complete-word # complete on tab, leave expansion to _expand

# Remove ctrl+y from the keybinds for delayed suspend
if [[ $OS == "Darwin" ]]; then
      stty dsusp undef
fi
