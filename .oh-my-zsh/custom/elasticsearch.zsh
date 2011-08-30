# shell aliases used for ES querying

# global alias for drew pasting
alias -g dpaste='curl -s p.draines.com'

function draines () {
    curl -s p.draines.com/$1 | zsh
}

function indices () {
    draines indices.sh
}

function indices15 () {
    curl -s p.draines.com/indices.sh | zsh -s 15
}

function shards () {
    draines shards.sh
}

function cluster () {
    draines cluster.sh
}

function health () {
    draines 131337422702048bcf752.txt
}

function health-nocolor () {
    draines health.sh
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

# indices15.sh
# shards.sh
# cluster.sh
# health.sh
# curl -s p.draines.com/health.py | python -
