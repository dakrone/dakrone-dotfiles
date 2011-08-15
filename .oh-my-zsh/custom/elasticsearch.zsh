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
    draines 1313372764486f99cf8b0.txt
}

function health-nocolor () {
    draines health.sh
}

# indices15.sh
# shards.sh
# cluster.sh
# health.sh
# curl -s p.draines.com/health.py | python -
