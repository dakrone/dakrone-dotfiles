# shell aliases used for ES querying

alias -g cs='curl -s'
alias -g csu='curl -s -u'

# HTTP verbs
alias get='curl -s -XGET'
alias post='curl -s -XPOST'
alias put='curl -s -XPUT'
alias delete='curl -s -XDELETE'

function dpaste () {
    zsh <(curl -s p.draines.com/sh) $1
}

function draines () {
    curl -s p.draines.com/$1 | zsh
}

function health () {
    curl -s p.draines.com/1336417317985b7d30212.txt | zsh
}

function jstk () {
    sudo -u nobody jstack $1 >jstack-$(date +%s).txt
}

function red () {
    curl -s p.draines.com/willbered.sh | zsh -s $1
}

function disk () {
    curl -s p.draines.com/disk.sh | zsh
}
