# knife things
export OPSCODE_USER="sonian_devs"
export ENV="dev"
export CHEF_USER="lee"

export SONIAN_DIR=~/safe
export SONIAN_USER=lee

# takes out the date because I know when today is
alias -g safetail="tail -F safe.log | sed 's/^20..-............................//;s/\| \[main\] \|//g'"

#lotus tests
#export LD_LIBRARY_PATH=/opt/ibm/lotus/notes/

function lotus () { export LD_LIBRARY_PATH=/opt/ibm/lotus/notes/ }
function nolotus () { unset LD_LIBRARY_PATH }

# public hostname for ec2 knife stuff
#function eknife () { knife $@ -a ec2.public_hostname -x lee }

if [[ ! -n $KNIFE_CMD ]]; then
    export KNIFE_CMD=`which knife`
fi

# function knife
# {
#     if [ $1 = "ssh" ]; then
#         $KNIFE_CMD "$@" -a ec2.public_hostname -x lee
#     else
#         $KNIFE_CMD "$@"
#     fi
# }

