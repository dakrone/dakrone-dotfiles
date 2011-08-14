# shell aliases used for ES querying

# global alias for drew pasting
alias -g dpaste='curl -s p.draines.com'

function draines () {
    curl -s p.draines.com/$@
}

function indices () {
    draines/indices15.sh
}

function shards () {
    draines/shards.sh
}

function cluster () {
    draines/cluster.sh
}

function health () {
    draines/health.sh
}

# indices15.sh
# shards.sh
# cluster.sh
# health.sh
# curl -s p.draines.com/health.py | python -
