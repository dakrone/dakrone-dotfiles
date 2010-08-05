# autoloads
autoload -U compinit
autoload -U promptinit
autoload colors
colors
compinit
promptinit

# path
export PATH=~/bin:/usr/local/bin:$PATH:/usr/local/sbin:/usr/local/sbin:/usr/libexec:/opt/local/bin:/opt/local/sbin:/usr/local/mysql/bin
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
# Path for liebke's clj - http://github.com/liebke/clj
export PATH=/Users/hinmanm/.cljr/bin:$PATH

# Chris' ruby stuff
export RUBYLIB=~/src/chrisbin/ruby
export RUBYOPT=rubygems
export PATH=$PATH:~/src/chrisbin:~/src/chrisbin/ruby

# I'm using java 1.6 on OSX
export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home

# VimClojure stuff (for nailgun server)
export CLOJURE_EXT=/Users/hinmanm/.vimclojure:/Users/hinmanm/.cljr/lib:./lib:./classes:./src:.
export CLOJURE_OPTS="-server -Xmx1024m -XX:+UseConcMarkSweepGC -XX:+HeapDumpOnOutOfMemoryError"

# cljr options
export JVM_OPTS="-server -Xmx1024m -XX:+UseConcMarkSweepGC -XX:+HeapDumpOnOutOfMemoryError"

# manpath
export MANPATH=$MANPATH:/usr/local/man:/opt/local/share/man

# abbreviation
export EDITOR=vim
export PAGER=less

# CVS for HeX
#export CVSROOT=:ext:dakrone@cvsup.rawpacket.org:/home/project/rawpacket/cvs

# CVS for Avamar
export CVSROOT=:ext:hinmam@cvs.avamar.com:/antimatter/home/codebase/repository
export CVS_RSH=ssh
export CVS_SERVER=/opt/sfw/bin/cvs
export CVSEDITOR=vim


# RSPEC for autotest
export RSPEC=true

# Source j.sh
source ~/bin/j.sh

# Resty
. ~/bin/resty

# rvm stuff:
if [ -s ~/.rvm/scripts/rvm ] ; then source ~/.rvm/scripts/rvm ; fi
# Set default ruby install
rvm default

# path for maven
export PATH=$PATH:/opt/apache-maven-2.2.1

# IRBRC
export IRBRC=~/.irbrc

# Perforce
export P4PORT=137.69.227.201:1666
export P4USER=hinmam
export P4CLIENT=hinmam_Xanadu
export P4DIFF=colordiff

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
alias gv='cd /media/VAULT/'
alias remhex='ssh -i ~/.ssh/id_rawpacket dakrone@localhost -p 6666'
alias remblack='ssh -i ~/.ssh/id_rawpacket hinmanm@localhost -p 7777'
alias remstyx='ssh -i ~/.ssh/id_rawpacket lee@localhost -p 6666'
alias scsetup='sudo socat -d -d TCP4-listen:6666,fork OPENSSL:typoet.com:443,cert=host.pem,verify=0'
alias scsetup2='sudo socat -d -d TCP4-listen:7777,fork OPENSSL:blackex:443,cert=host.pem,verify=0'
alias blackexprox='ssh -i ~/.ssh/id_rawpacket -ND 9999 hinmanm@localhost -p 7777'
alias blackprox='ssh -i ~/.ssh/id_rawpacket -ND 9999 hinmanm@black'
alias styxprox='ssh -i ~/.ssh/id_rawpacket -ND 9999 lee@localhost -p 6666'
alias tcpdump='tcpdump -ttttnnn'
alias vless=/usr/share/vim/vim72/macros/less.sh
alias week='remind -c+1 ~/.reminders'
alias month='remind -c ~/.reminders'
alias gps='geektool-ps'
alias jl='j --l'
alias jr='j --r'
alias js='j --s'
alias givm='gvim'
# Hate perforce.
alias pd='p4 diff -du'
alias pc='p4 changes //dtlt/... | maxhead'
alias pac='p4 changes //aam/... | maxhead'
# Colored rspec
alias cspec='spec -c --format specdoc'
# Tmux stuff
# force 256 color mode
alias tmux='tmux -2'
alias screen='TERM=xterm-color && /opt/local/bin/screen'
alias rvim='gvim --remote-tab-silent'
alias todo='rvim ~/vimwiki/ToDo.wiki'
alias msync='rsync -av --ignore-existing --delete ~/Music/iTunes/iTunes\ Music/* dagger:~/Music/'


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
# Perforce describe (with color)
function pdesc() {
      p4 describe $1 | p4c
}


# prompt (if running screen, show window #)
#if [ x$WINDOW != x ]; then
    # [5:hinmanm@dagger:~]% 
    #export PS1="%{$fg[blue]%}[%{$fg[cyan]%}$WINDOW%{$fg[blue]%}:%{$fg[green]%}%n%{$fg[cyan]%}@%{$fg[green]%}%m%{$fg[blue]%}:%{$fg[magenta]%}%~%{$fg[blue]%}]%{$reset_color%}%# "
    #export PS1="%{$fg[magenta]%}$WINDOW %{$fg[blue]%}‹ %{$fg[green]%}%~%{$fg[blue]%} › %{$fg[green]%}∴%{$reset_color%} "
#else
    # [hinmanm@dagger:~]% 
    #export PS1="%{$fg[blue]%}[%{$fg[green]%}%n%{$fg[cyan]%}@%{$fg[green]%}%m%{$fg[blue]%}:%{$fg[magenta]%}%~%{$fg[blue]%}]%{$reset_color%}%# "
    export PS1="%{$fg[grey]%}‹ %{$fg[blue]%}%~%{$fg[grey]%} › %{$fg[green]%}∴%{$reset_color%} "
#fi
export RPRMOPT="%{$reset_color%}"

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

  # Print the regular prompt, or the lightning bolt if uncommitted git files
  #flag=`git_dirty_flag`
  #if [ -n "$flag" ]; then
    #export PS1="$PS1`git_dirty_flag`"
  #else
    #export PS1="$PS1%# "
  #fi
}

# preexec is called just before any command line is executed
function preexec() {
  title "$1" "$USER@%m" "%35<...<%~"
}

## For the "ZoomGo" ruby file
function zg () {
  eval cd `zg.rb $1`
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
bindkey "^b" backward-word
bindkey "^f" forward-word
bindkey "^d" delete-word
bindkey "^k" kill-line
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^I' complete-word # complete on tab, leave expansion to _expand

# Remove ctrl+y from the keybinds for delayed suspend
stty dsusp undef
