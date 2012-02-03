# set aliases
# no spelling correction on mv
alias mv='nocorrect mv'
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
alias j='z'
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
alias rsynco='rsync --compress-level=9 -azvvPhSImi'
alias fa=fix-agent
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
# keep an X connection open, with proxy
alias keepircprox='autossh -M 21000 irc.sa2s.us -L 6667:irc.sa2s.us:31425'
# keep an X connection open, without proxy
alias keepx='autossh -M 22000 x'
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

# Stolen from Decklin
alias e='$EDITOR'
alias m='$PAGER'
alias h='fc -l'
alias g='egrep -i'
alias rg='egrep -ir'
alias v='egrep -iv'
alias gf='fgrep -f'
alias vf='fgrep -vf'

# knife stuff
alias -g kssh='knife sq ssh'
alias -g kps='knife sq ps'
alias -g kst='knife sq status'
alias -g kex='knife sq exec'
