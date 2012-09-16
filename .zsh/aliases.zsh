# Aliases

# no spelling correction on mv
alias mv='nocorrect mv'
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'
# colorful ls for whichever platform
if ls -F --color=auto >&/dev/null; then
    alias ls="ls --color=auto -F"
else
    alias ls="ls -GF"
fi
# dfc - http://projects.gw-computing.net/projects/dfc
if dfc >&/dev/null; then
    alias df="dfc"
fi
# various ls helpers
alias l.='ls -d .*'
alias ll='ls -lh'
alias la='ls -alh'
alias lr='ls -lR'
# for sl: http://practicalthought.com/sl/
alias ss='sl'
# if you have ls++, uncomment this
# alias ll='ls++'
# alias la='ls++ -a'
# quickjump to directories
alias j='z'
# various jump options
alias jl='j --l'
alias jr='j --r'
alias js='j --s'
# don't page unless needed
alias less='less -FRX'
# colorize greps
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias g='fgrep -i --color=auto'
# cd helpers
alias cd..='cd ..'
alias ..='cd ..'
# jump to nsm-console
alias nsmc='cd ~/src/ruby/nsm-console && ./nsm'
# look up a service port
alias serv='cat /etc/services | grep'
# look up a process quickly
function pg {
    # doing it again afterwards for the coloration
    ps aux | fgrep -i $1 | fgrep -v fgrep | fgrep -i $1
}
# eh, who likes typing sudo anyway?
alias dmesg='sudo dmesg'
# various port forwarding and hole-punching
alias remhex='ssh -i ~/.ssh/id_rawpacket dakrone@localhost -p 6666'
alias remblack='ssh -i ~/.ssh/id_rawpacket hinmanm@localhost -p 7777'
alias remstyx='ssh -i ~/.ssh/id_rawpacket lee@localhost -p 6666'
alias scsetup='sudo socat -d -d TCP4-listen:6666,fork OPENSSL:typoet.com:443,cert=host.pem,verify=0'
alias scsetup2='sudo socat -d -d TCP4-listen:7777,fork OPENSSL:blackex:443,cert=host.pem,verify=0'
alias blackexprox='ssh -i ~/.ssh/id_rawpacket -ND 9999 hinmanm@localhost -p 7777'
alias blackprox='ssh -i ~/.ssh/id_rawpacket -ND 9999 hinmanm@black'
alias styxprox='ssh -i ~/.ssh/id_rawpacket -ND 9999 lee@localhost -p 6666'
# keep an X connection open, with proxy
alias keepircprox='autossh -M 21000 corinth -L 6667:corinth:31425'
# keep an X connection open, without proxy
alias keepx='autossh -M 22000 x'
alias keepc='autossh -M 22000 corinth'
# reverse proxy & keepopen
alias xprox='ssh -nNT -R 4444:localhost:22 x'
alias cprox='ssh -nNT -R 4444:corinth:22 x'
alias prox='ssh -nNT -R 4444:localhost:22 localhost'
alias autoxprox='autossh -M 22000 -nNT -R 4444:localhost:22 x'
alias autocprox='autossh -M 22000 -nNT -R 4444:corinth:22 x'
alias autoprox='autossh -M 22000 -nNT -R 4444:localhost:22 localhost'
alias recon='autossh -M 21234 ssh corinth'
# tcpdump default opts
alias tcpdump='tcpdump -ttttnnn'
# vim less options
alias vless=/usr/share/vim/vim72/macros/less.sh
# correct mistypes for gvim
alias givm='gvim'
# remote gvim tab
alias rvim='gvim --remote-tab-silent'
# better rsync defaults
alias rsynco='rsync --checksum --delete -aPhSImi'
# fix ssh agent
alias fa=fix-agent
# always try to fix our agent if we can before sshing
#alias ssh="fix-agent; ssh"
# Colored rspec
alias cspec='spec -c --format specdoc'
# Tmux stuff
# force 256 color mode
alias tmux='tmux -2'
# screen, although who uses screen anymore?
alias screen='TERM=xterm-color && /opt/local/bin/screen'
# open work file really quickly
alias todo='ec -n ~/work.org'
# open elinks quickly
alias el='TERM=xterm-color elinks'
# autossh stuff
alias -g ash='autossh'
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
# classpath reading! (wheeeeeee...)
alias rcp="tr ':' '\n'"
# lein aliases
alias lr='lein repl'
alias ldt='lein difftest'

# knife stuff
alias -g kssh='knife sq ssh'
alias -g kps='knife sq ps'
alias -g kst='knife sq status'
alias -g kex='knife sq exec'

# global json prettification
alias -g pj='python -mjson.tool'
