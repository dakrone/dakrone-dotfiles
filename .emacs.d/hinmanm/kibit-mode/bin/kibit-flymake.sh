# Setting basedir on flymake tasks doesn't actually get set beyond a
# logging statement, so we hijack the CWD using this script to farm
# out the job to leiningen

# normalize $0 on certain BSDs
if [ "$(dirname "$0")" = "." ]; then
    SCRIPT="$(which $(basename "$0"))"
else
    SCRIPT="$0"
fi

cd "$(dirname "$(dirname "$SCRIPT")")"
lein run -m kibit-mode.flymake $1
