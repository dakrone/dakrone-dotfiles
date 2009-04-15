#!/bin/sh

# Please make sure to configure ~/.clojure.conf or /etc/clojure.conf
#  sample configuration can be found at clojure.conf.sample
#
# Note, running this script will:
#   - Run ~/.clojurerc on boot up (if exists)
#   - Add all .jar files within clj_ext (~/.clojure on default)
#     to the classpath
#
#

if [ ! -f /etc/clojure.conf -a ! -f ~/.clojure.conf ]; then
    echo "Error: No config not found at /etc/clojure.conf or ~/.clojure.conf."
    echo "  Please provide one before starting this script."
    echo "  A sample can be found in the emacs-clojure repository named "
    echo "   clojure.conf.sample"
    exit
fi


# Whether to load the repl or script
if [ -z "$1" ]; then
	clj_class=clojure.lang.Repl
else
	clj_class=clojure.lang.Script
fi

clj_cp="."
[ -f /etc/clojure.conf ] && . /etc/clojure.conf
[ -f ~/.clojure.conf ]   && . ~/.clojure.conf
[ -f ~/.clojurerc ] && clj_rc=~/.clojurerc
[ -d "${clj_ext}" ] && clj_cp="${clj_cp}:${clj_ext}/*"

if [ -n "${clj_lib}" ]; then
    export LD_LIBRARY_PATH=${clj_lib}:$LD_LIBRARY_PATH
fi

exec java -Dpid=$$ ${clj_opts} -cp ${clj_cp}:${clj} ${clj_wrapper} ${clj_class} ${clj_rc} $*
