# Things for gtags/global

if [ -f ~/.globalrc ]; then
    export GTAGSCONF=$HOME/.globalrc
elif [ -f /usr/local/share/gtags/gtags.conf ] ; then
    export GTAGSCONF=/usr/local/share/gtags/gtags.conf
fi

export GTAGSLABEL=ctags
