# Aliases

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
alias l='ls -lh'
alias la='ls -alh'
alias lr='ls -lR'
# colorize greps
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

alias less='less -RX'

# cd helpers
alias ..='cd ..'
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
alias keepbprox='autossh -M 23000 corinth -L 7778:corinth:6667'
# keep an X connection open, without proxy
alias keepx='autossh -M 22000 x'
alias keepc='autossh -M 22000 corinth'
alias keepivr='autossh -M 22000 ivalice-remote'
alias keepivl='autossh -M 22000 ivalice-local'
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
# open elinks quickly
alias el='TERM=xterm-color elinks'
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
# classpath reading! (wheeeeeee...)
alias rcp="tr ':' '\n'"

# start a master tmux
alias tm='tmux -2 -u -S /tmp/mastermux -f .tmux.master'

# datetime aliases
alias dt='gdate "+%Y-%m-%dT%H:%M:%S.%3N%zZ"'
# Elasticsearch's basic_date_time
alias bdt='gdate "+%Y%m%dT%H%M%S.%3N%z"'
alias epoch='date +%s'
# jump start to magit
alias magit='emacs -f magit-status'
# npm install -g gitjk
alias gitjk='history | tail -r | head -10 | gitjk_cmd'
