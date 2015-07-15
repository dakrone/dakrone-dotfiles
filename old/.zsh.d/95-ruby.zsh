# RVM
# if [ -s ~/.rvm/scripts/rvm ] ; then
#     source ~/.rvm/scripts/rvm
#     # Set default ruby install
#     rvm default
#     PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
# fi

# Rbenv
# if [ -s /usr/local/var/rbenv ]; then
#     # To use Homebrew's directories rather than ~/.rbenv add to your profile:
#     export RBENV_ROOT=/usr/local/var/rbenv
# fi

# # To enable shims and autocompletion add to your profile:
# if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# rbenv install 1.9.3-p547
# echo "1.9.3-p547" > ~/.rbenv/version

# Chruby
if [ -s /usr/local/opt/chruby/share/chruby/chruby.sh ]; then
    source /usr/local/opt/chruby/share/chruby/chruby.sh
    chruby ruby-2.1.3
fi
