# keymap

bindkey -e
bindkey "^?"    backward-delete-char
bindkey "^H"    backward-delete-char
bindkey "^[[3~" delete-char
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line
##
## Spoonfork's bindkeys
##
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


# history
HISTFILE=$HOME/.zsh-history
HISTSIZE=2000
SAVEHIST=2000
setopt extended_history
setopt share_history
function history-all { history -E 1 }

# path
export PATH=~/bin:$PATH:/usr/local/sbin:/usr/local/bin:/usr/local/sbin:/usr/libexec:/opt/local/bin:/opt/local/sbin:/usr/local/mysql/bin
# Path for Matasano's blackbag
export PATH=/usr/local/bin/blackbag:$PATH
# Path for git
export PATH=$PATH:/usr/local/git/bin

# manpath
export MANPATH=$MANPATH:/usr/local/man:/opt/local/share/man

# fink
test -r /sw/bin/init.sh && . /sw/bin/init.sh

# abbreviation
export EDITOR=vim
export PAGER=less

# CVS for HeX
export CVSROOT=:ext:dakrone@cvsup.rawpacket.org:/home/project/rawpacket/cvs

# set aliases
alias ls='ls -G'
alias l.='ls -d .*'
alias ll='ls -lh'
alias la='ls -alh'
alias lr='ls -lR'
alias less='less -FRX'
alias grep='egrep -i --color=auto'
alias ..='cd ..'
alias nsmc='cd ~/hex/hex/rawpacket-root/usr/home/analyzt/rp-NSM/nsm-console'
alias archup='rsync -e "ssh -p 49007 -i /Users/hinmanm/.ssh/id_rsa_rawpacket -l rawpacket" -av 143.215.139.8:archives archives'
alias currup='rsync -e "ssh -p 49007 -i /Users/hinmanm/.ssh/id_rsa_rawpacket -l rawpacket" -av 143.215.139.8:current current'
alias serv='cat /etc/services | grep'
alias pg='ps aux | grep'
alias nl='netstat -tlna | grep -v "stream|dgram"'
alias dmesg='sudo dmesg'
alias gv='cd /Volumes/VAULT/'
alias remhex='ssh -i ~/.ssh/id_rawpacket dakrone@localhost -p 6666'
alias aremhex='autossh -M 20000 -i ~/.ssh/id_rawpacket dakrone@localhost -p 6666'
alias scsetup='sudo socat -d -d TCP4-listen:6666,fork OPENSSL:hexbit:443,cert=host.pem,verify=0'
alias rbsync='rsync -artv "/Users/hinmanm/Music/iTunes/iTunes Music" /Volumes/ALTHALUS/Music/'

# aliases for MacVim
alias gvim='open -a MacVim'
alias mvim='open -a MacVim'

# use geektool-ps and geektool-network
alias gps='geektool-ps'
alias gnw='geektool-network'

# use vim as a pager
alias vless=/usr/share/vim/vim70/macros/less.sh

# Use the screen from MacPorts
# Not necessary any more since I got it from CVS and patched it myself
#alias screen='/opt/local/bin/screen'
# Don't resolv names in tcpdump
alias tcpdump='tcpdump -n'
# Use exuberant ctags
alias ctags='/usr/local/bin/ctags'
# alias for starting synergy server
alias ssyn='synergys -f -n euclid'


### OPTIONS ###
setopt PROMPT_SUBST
setopt autopushd
setopt NOTIFY
#setopt NO_FLOW_CONTROL
setopt APPEND_HISTORY
# setopt AUTO_LIST		# these two should be turned off
# setopt AUTO_REMOVE_SLASH
# setopt AUTO_RESUME		# tries to resume command of same name
unsetopt BG_NICE		# do NOT nice bg commands
setopt EXTENDED_HISTORY		# puts timestamps in the history
# setopt HASH_CMDS		# turns on hashing
setopt HIST_ALLOW_CLOBBER
setopt HIST_REDUCE_BLANKS
setopt INC_APPEND_HISTORY SHARE_HISTORY
setopt ALL_EXPORT

# Set/unset  shell options
setopt   notify globdots pushdtohome cdablevars autolist
setopt   autocd recexact longlistjobs
setopt   autoresume histignoredups pushdsilent noclobber
setopt   autopushd pushdminus extendedglob rcquotes mailwarning
unsetopt bgnice autoparamslash

# Autoload zsh modules when they are referenced
zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -a zsh/zprof zprof
zmodload -ap zsh/mapfile mapfile

# completion
autoload -U compinit
compinit
##
## Spoonfork's completion
##
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:processes' command 'ps -axw'
zstyle ':completion:*:processes-names' command 'ps -awxho command'
# Completion Styles
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
# list of completers to use
#zstyle ':completion:*::::' completer _expand _complete _ignored _approximate
#NEW completion:
# 1. All /etc/hosts hostnames are in autocomplete
# 2. If you have a comment in /etc/hosts like #%foobar.domain,
#    then foobar.domain will show up in autocomplete!
zstyle ':completion:*' hosts $(awk '/^[^#]/ {print $2 $3" "$4" "$5}' /etc/hosts | grep -v ip6- && grep "^#%" /etc/hosts | awk -F% '{print $2}') 
# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
# offer indexes before parameters in subscripts
#zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters
# command for process lists, the local web server details and host completion
zstyle '*' hosts $hosts
# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:scp:*' tag-order \
   files users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:scp:*' group-order \
   files all-files users hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order \
   users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:ssh:*' group-order \
   hosts-domain hosts-host users hosts-ipaddr
#zstyle '*' single-ignored show

#if [ -z "$LINES" ] || ! ( echo $LINES | grep -q '^[0-9]\+$' ) ; then
#      LINES=20
#fi

function maxhead() { head -n `echo $LINES - 5|bc` ; }
function maxtail() { tail -n `echo $LINES - 5|bc` ; }

## For the "ZoomGo" ruby file
function zg () {
  eval cd `zg.rb $1`
}


###############################################################################
##                                 PROMPT                                    ##
###############################################################################
function precmd {

    local TERMWIDTH
    (( TERMWIDTH = ${COLUMNS} - 1 ))


    ###
    # Truncate the path if it's too long.
    
    PR_FILLBAR=""
    PR_PWDLEN=""
    
    local promptsize=${#${(%):---(%n@%m:%l)---()--}}
    local pwdsize=${#${(%):-%~}}
    
    if [[ "$promptsize + $pwdsize" -gt $TERMWIDTH ]]; then
	    ((PR_PWDLEN=$TERMWIDTH - $promptsize))
    else
	PR_FILLBAR="\${(l.(($TERMWIDTH - ($promptsize + $pwdsize)))..${PR_HBAR}.)}"
    fi


    ###
    # Get APM info.

    if which ibam > /dev/null; then
	PR_APM_RESULT=`ibam --percentbattery`
    elif which apm > /dev/null; then
	PR_APM_RESULT=`apm`
    fi
}


setopt extended_glob
preexec () {
    if [[ "$TERM" == "screen" ]]; then
	local CMD=${1[(wr)^(*=*|sudo|-*)]}
	echo -n "\ek$CMD\e\\"
    fi
}


setprompt () {
    ###
    # Need this so the prompt will work.

    setopt prompt_subst


    ###
    # See if we can use colors.

    autoload colors zsh/terminfo
    if [[ "$terminfo[colors]" -ge 8 ]]; then
	colors
    fi
    for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
	#eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
	eval PR_$color='%{$fg[${(L)color}]%}'
	eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
	(( count = $count + 1 ))
    done
    PR_NO_COLOUR="%{$terminfo[sgr0]%}"


    ###
    # See if we can use extended characters to look nicer.
    
    typeset -A altchar
    set -A altchar ${(s..)terminfo[acsc]}
    PR_SET_CHARSET="%{$terminfo[enacs]%}"
    PR_SHIFT_IN="%{$terminfo[smacs]%}"
    PR_SHIFT_OUT="%{$terminfo[rmacs]%}"
    PR_HBAR=${altchar[q]:--}
    PR_ULCORNER=${altchar[l]:--}
    PR_LLCORNER=${altchar[m]:--}
    PR_LRCORNER=${altchar[j]:--}
    PR_URCORNER=${altchar[k]:--}

    
    ###
    # Decide if we need to set titlebar text.
    
    case $TERM in
	xterm*)
	    PR_TITLEBAR=$'%{\e]0;%(!.-=*[ROOT]*=- | .)%n@%m:%~ | ${COLUMNS}x${LINES} | %y\a%}'
	    ;;
	screen)
	    PR_TITLEBAR=$'%{\e_screen \005 (\005t) | %(!.-=[ROOT]=- | .)%n@%m:%~ | ${COLUMNS}x${LINES} | %y\e\\%}'
	    ;;
	*)
	    PR_TITLEBAR=''
	    ;;
    esac
    
    
    ###
    # Decide whether to set a screen title
    if [[ "$TERM" == "screen" ]]; then
	PR_STITLE=$'%{\ekzsh\e\\%}'
    else
	PR_STITLE=''
    fi
    
    
    ###
    # APM detection
    
    if which ibam > /dev/null; then
	PR_APM='$PR_RED${${PR_APM_RESULT[(f)1]}[(w)-2]}%%(${${PR_APM_RESULT[(f)3]}[(w)-1]})$PR_LIGHT_BLUE:'
    elif which apm > /dev/null; then
	PR_APM='$PR_RED${PR_APM_RESULT[(w)5,(w)6]/\% /%%}$PR_LIGHT_BLUE:'
    else
	PR_APM=''
    fi
    
    
    ###
    # Finally, the prompt.

    PROMPT='$PR_SET_CHARSET$PR_STITLE${(e)PR_TITLEBAR}\
$PR_CYAN$PR_SHIFT_IN$PR_ULCORNER$PR_BLUE$PR_HBAR$PR_SHIFT_OUT(\
$PR_GREEN%(!.%SROOT%s.%n)$PR_GREEN@%m:%l\
$PR_BLUE)$PR_SHIFT_IN$PR_BLUE$PR_SHIFT_OUT(\
$PR_MAGENTA%$PR_PWDLEN<...<%~%<<\
$PR_BLUE)$PR_SHIFT_OUT\

$PR_CYAN$PR_SHIFT_IN$PR_LLCORNER$PR_BLUE$PR_HBAR$PR_SHIFT_OUT(\
%(?..$PR_LIGHT_RED%?$PR_BLUE:)\
${(e)PR_APM}$PR_GREEN%D{%H:%M}\
$PR_LIGHT_BLUE:%(!.$PR_RED.$PR_WHITE)%#$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_NO_COLOUR '

#    RPROMPT=' $PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_BLUE$PR_HBAR$PR_SHIFT_OUT\
#($PR_GREEN%D{%a,%b%d}$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_CYAN$PR_LRCORNER$PR_SHIFT_OUT$PR_NO_COLOUR'
RPROMPT=''

    PS2='$PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_BLUE$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT(\
$PR_LIGHT_GREEN%_$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT$PR_NO_COLOUR '
}

setprompt
###############################################################################
##                               END PROMPT                                  ##
###############################################################################
