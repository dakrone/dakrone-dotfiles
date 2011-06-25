OS=$(uname)

# path
export PATH=/usr/local/bin:$PATH:/usr/local/sbin:/usr/local/sbin:/usr/libexec:/opt/local/sbin:/usr/local/mysql/bin

# Java opts (leiningen uses these)
export JAVA_OPTS="-server -Dfile.encoding=UTF-8 -Dslime.encoding=UTF-8 -Xmx512m -XX:+HeapDumpOnOutOfMemoryError"

# manpath
export MANPATH=$MANPATH:/usr/local/man:/opt/local/share/man

# abbreviation
export EDITOR=vim
export PAGER=less

# knife things
export OPSCODE_USER="sonian_developer"
export ENV="dev"
export CHEF_USER="lee"

# report things that take a while
export REPORTTIME=5

# node things
export NODE_PATH=/usr/local/lib/node
export PATH=$PATH:/usr/local/share/npm/bin
# npm will install libraries to:
#   /usr/local/lib/node/.npm
# To manually remove libraries installed by npm, delete this (hidden!) folder.

# CVS options
export CVSEDITOR=vim

# RSPEC for autotest
export RSPEC=true

# ledger
export LEDGER_FILE=~/data/ledger.dat

# Source z.sh
if [ -s ~/bin/z.sh ] ; then
    source ~/bin/z.sh ;
    function precmd () {
        z --add "$(pwd -P)"
        # oh-my-zsh stuff
        title $ZSH_THEME_TERM_TAB_TITLE_IDLE $ZSH_THEME_TERM_TITLE_IDLE
    }
    alias j='z'
fi

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

# set aliases
alias mv='nocorrect mv'       # no spelling correction on mv
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'
if ls -F --color=auto >&/dev/null; then
  alias ls="ls --color=auto -F"
else
  alias ls="ls -GF"
fi
alias l.='ls -d .*'
alias ll='ls -lh'
alias la='ls -alh'
alias lr='ls -lR'
# if you have ls++, uncomment this
# alias ll='ls++'
# alias la='ls++ -a'

alias less='less -FRX'
alias grep='grep -i --color=auto'
alias egrep='egrep -i --color=auto'
alias cd..='cd ..'
alias ..='cd ..'
alias nsmc='cd ~/src/ruby/nsm-console'
alias serv='cat /etc/services | grep'
alias pg='ps aux | grep'
alias dmesg='sudo dmesg'
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
alias screen='TERM=xterm-color && /opt/local/bin/screen'
alias todo='ec -n ~/work.org'
# elinks stuff
alias el='TERM=xterm-color elinks'
# autossh stuff
alias -g ash='autossh'
# 20 second poll time
export AUTOSSH_POLL=20
# keep an X connection open
alias keepx='autossh -M 21000 x -L 6667:x:31425'
# reverse proxy & keepopen
alias xprox='ssh -nNT -R 4444:localhost:22 x'
alias prox='ssh -nNT -R 4444:localhost:22 localhost'
alias autoxprox='autossh -M 22000 -nNT -R 4444:localhost:22 x'
alias autoprox='autossh -M 22000 -nNT -R 4444:localhost:22 localhost'
## ledger aliases
# balance
alias bal='ledger -s -V bal'
# net between assets & liabilities
alias netbal='ledger -s bal \^assets \^liab'
# uncleared transactions
alias uc='ledger -U reg'
# show budgets starting in march (march is the first month I had complete
# transactions for the whole month
alias budget='ledger --budget -b Mar -M reg expenses'
alias ytdbug='ledger -M -b Mar budget'

# history
HISTFILE=$HOME/.zsh-history
HISTSIZE=5000
SAVEHIST=1000
setopt appendhistory autocd extendedglob
setopt share_history
function history-all { history -E 1 }

# functions
setenv() { export $1=$2 }  # csh compatibility
rot13 () { tr "[a-m][n-z][A-M][N-Z]" "[n-z][a-m][N-Z][A-M]" }
function maxhead() { head -n `echo $LINES - 5|bc` ; }
function maxtail() { tail -n `echo $LINES - 5|bc` ; }
function bgrep() { git branch -a | grep "$*" | sed 's,remotes/,,'; }

# public hostname for ec2 knife stuff
function eknife () { knife $@ -a ec2.public_hostname -x lee }

# function to fix ssh agent
function fix-agent() {
    export SSH_AUTH_SOCK=`ls -t1 $(find /tmp/ -uid $UID -path \\*ssh\\* -type s 2> /dev/null) | head -1`
}

# colorful listings
zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' hosts $ssh_hosts
zstyle ':completion:*:my-accounts' users-hosts $my_accounts
zstyle ':completion:*:other-accounts' users-hosts $other_accounts

### OPTIONS ###
unsetopt BG_NICE             # do NOT nice bg commands
unsetopt correct_all         # don't correct me, I know what I'm doing
setopt EXTENDED_HISTORY      # puts timestamps in the history
setopt NO_HUP                # don't send kill to background jobs when exiting

# Keybindings
# Emacs keybindings
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
#bindkey "^d" delete-word
bindkey "^k" kill-line
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^I' complete-word # complete on tab, leave expansion to _expand
bindkey -r '^j' #unbind ctrl-j, I hit it all the time accidentaly

# word chars
# default is: *?_-.[]~=/&;!#$%^(){}<>
export WORDCHARS="*?_-[]~=&!#$%^(){}<>"

# normal ls colors
unset LSCOLORS

if [[ "$TERM" == "dumb" ]] then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  unfunction precmd
  unfunction preexec
  export PS1='$ '
fi
