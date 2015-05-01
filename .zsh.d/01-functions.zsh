# Zsh functions

not_in_emacs() {
    [[ ! -n $EMACS ]]
}

not_in_cloud() {
    ! fgrep -q ami_id /etc/motd 2>/dev/null
}

# functions
function history-all { history -E 1 }
function setenv() { export $1=$2 }  # csh compatibility
function rot13 () { tr "[a-m][n-z][A-M][N-Z]" "[n-z][a-m][N-Z][A-M]" }
function maxhead() { head -n `echo $LINES - 5|bc` ; }
function maxtail() { tail -n `echo $LINES - 5|bc` ; }
function bgrep() { git branch -a | grep "$*" | sed 's,remotes/,,'; }

# function to fix ssh agent
function fix-agent() {
    disable -a ls
    export SSH_AUTH_SOCK=`ls -t1 $(find /tmp/ -uid $UID -path \\*ssh\\* -type s 2> /dev/null) | head -1`
    enable -a ls
}

# define a word
function define(){
    if [[ $# -ge 2 ]] then
       echo "givedef: too many arguments" >&2
       return 1
       else
           curl "dict://dict.org/d:$1"
    fi
}

## TODO make these scripts instead of functions

# Check if a URL is up
function chk-url() {
    curl -sL -w "%{http_code} %{url_effective}\\n" "$1" -o /dev/null
}

function pub() {
    scp $1 writequit:public_html/wq/pub/
}

# Check CLA status for ES pull requests
function cla() {
    curl -I "http://54.204.36.1:3000/verify/nickname/$1"
}

# Tunnel ES from somewhere to here locally on port 9400
function es-tunnel() {
    autossh -M0 $1 -L 9400:localhost:9200 -CNf
}

# Tunnel logstash/kibana locally
function kibana-tunnel() {
    autossh -M0 $1 -L 9292:localhost:9292 -CNf
}

# Delete a branch locally and on my (dakrone) fork
function del-branch() {
    git branch -D $1
    git push dakrone :$1
}

# Fuzzy-check-out, check out the first local branch that matches
function fco() {
    git checkout `git branch | grep -i $1 | head -1 | tr -d " "`
}

# look up a process quickly
function pg {
    # doing it again afterwards for the coloration
    ps aux | fgrep -i $1 | fgrep -v fgrep | fgrep -i $1
}

function in_emacs {
    [[ -n $EMACS ]]
}

# cd back up to the highest level git repo dir
# thanks Dan!
function cds () {
    ORIGINAL_PWD=`pwd`
    while [ ! -d ".git" -a `pwd` != "/" ]
    do
        cd ..
    done
    if [ ! -d ".git" ]
    then
        cd $ORIGINAL_PWD
    fi
}
