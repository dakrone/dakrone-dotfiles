# rvm stuff (if it exists):
if [ -s ~/.rvm/scripts/rvm ] ; then
    source ~/.rvm/scripts/rvm
    # Set default ruby install
    rvm default
    PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
fi

# rbenv stuff
# if [ -s ~/.rbenv ]; then
#     eval "$(rbenv init -)"
# fi
