# Provide higer-order functions
# Taken from:
# http://yannesposito.com/Scratch/en/blog/Higher-order-function-in-zsh/

# usage:
#
# $ foo(){print "x: $1"}
# $ map foo a b c d
# x: a
# x: b
# x: c
# x: d
function map {
    local func_name=$1
    shift
    for elem in $@; print -- $(eval $func_name $elem)
}

# $ bar() { print $(($1 + $2)) }
# $ fold bar 0 1 2 3 4 5
# 15
# -- but also
# $ fold bar 0 $( seq 1 100 )
function fold {
    if (($#<2)) {
            print -- "ERROR fold use at least 2 arguments" >&2
            return 1
        }
        if (($#<3)) {
                print -- $2
                return 0
            } else {
                local acc
                local right
                local func_name=$1
                local init_value=$2
                local first_value=$3
                shift 3
                right=$( fold $func_name $init_value $@ )
                acc=$( eval "$func_name $first_value $right" )
                print -- $acc
                return 0
            }
}

# usage:
#
# $ baz() { print $1 | grep baz }
# $ filter baz titi bazaar biz
# bazaar
function filter {
    local predicate=$1
    local result
    typeset -a result
    shift
    for elem in $@; do
        if eval $predicate $elem >/dev/null; then
            result=( $result $elem )
        fi
    done
    print $result
}
