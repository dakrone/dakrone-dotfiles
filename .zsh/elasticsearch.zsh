# shell aliases used for ES querying

alias -g cs='curl -s'

function dpaste () {
    zsh <(curl -s p.draines.com/sh) $1
}

function draines () {
    curl -s p.draines.com/$1 | zsh
}

function indices () {
    curl -s p.draines.com/indices.sh | zsh
}

function indices15 () {
    curl -s p.draines.com/indices.sh | zsh -s 15
}

function shards () {
    curl -s p.draines.com/shards.sh | zsh
}

function cluster () {
    curl -s p.draines.com/cluster.sh | zsh
}

function health () {
    curl -s p.draines.com/131337422702048bcf752.txt | zsh
}

function health-nocolor () {
    curl -s p.draines.com/health.sh | zsh
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

function make-clean-shards () {
    curl -s p.draines.com/clean-shards.sh | zsh
}

function escolor () {
    es indices > >(fgrep red | wc -l | awk '{ print "red: " $0 }') > >(fgrep yellow | wc -l | awk '{ print "yellow: " $0 }') > >(fgrep green | wc -l | awk '{ print "green: " $0 }')
}

