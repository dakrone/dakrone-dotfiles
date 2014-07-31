# shell aliases used for ES querying

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
