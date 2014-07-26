echo -n "+++Reading .zshrc"
[[ -o interactive ]] && echo -n " (for interactive use)"
echo .

# Used for reporting how load loading takes
zmodload zsh/datetime
start=$EPOCHREALTIME

# path
export PATH=~/bin:/usr/local/bin:$PATH

# abbreviation
export EDITOR=nano # to be overwritten later
export PAGER=less

# report things that take more than 5 seconds
export REPORTTIME=5

# 10 second poll time for autossh
export AUTOSSH_POLL=10

# don't show load in prompt by default
export SHOW_LOAD=false

# start with a pre-title of nothing
export PRETITLE=""

# word chars
# default is: *?_-.[]~=/&;!#$%^(){}<>
# other: "*?_-.[]~=&;!#$%^(){}<>\\"
export WORDCHARS='*?_[]~=&;!#$%^(){}'

# history
HISTFILE=$HOME/.zsh-history
HISTSIZE=10000
SAVEHIST=5000
# "persistent history"
# just write important commands you always need to ~/.important_commands
if [[ -r ~/.important_commands ]] ; then
    fc -R ~/.important_commands
fi

# support colors in less
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# zsh completion
if [ -d ~/.zsh/zsh-completions ] ; then
    fpath=(~/.zsh/zsh-completions/src $fpath)
fi

autoload -U compinit zrecompile

zsh_cache=${HOME}/.zsh-cache
if [ $UID -eq 0 ]; then
    compinit
else
    compinit -d $zsh_cache/zcomp-$HOST

    for f in ~/.zshrc $zsh_cache/zcomp-$HOST; do
        zrecompile -p $f && rm -f $f.zwc.old
    done
fi

zstyle ':completion:::::' completer _complete _approximate
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh-cache
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' hosts $ssh_hosts
zstyle ':completion:*:my-accounts' users-hosts $my_accounts
zstyle ':completion:*:other-accounts' users-hosts $other_accounts
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX + $#SUFFIX) / 3 )) )'
zstyle ':completion:*:descriptions' format "- %d -"
zstyle ':completion:*:corrections' format "- %d - (errors %e})"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
zstyle ':completion:*' verbose yes
zstyle ':completion:*' file-list list=20 insert=10


### OPTIONS ###
setopt multios               # allow pipes to be split/duplicated
# ^^ try this: cat foo.clj > >(fgrep java | wc -l) > >(fgrep copy | wc -l)
setopt auto_cd
setopt extended_glob
setopt append_history
setopt extended_history
setopt share_history
setopt histignorealldups
setopt nohup
setopt longlistjobs
setopt notify
# I use dvorak, so correct spelling mistakes that a dvorak user would make
setopt dvorak

autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Keybindings
# Emacs keybindings
bindkey -e
bindkey "^?"    backward-delete-char
bindkey "^H"    backward-delete-char
bindkey "^[[3~" backward-delete-char
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line

bindkey '^r' history-incremental-search-backward
bindkey "^[[5~" up-line-or-history
bindkey "^[[6~" down-line-or-history
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
bindkey "^W" backward-delete-word
bindkey "^k" kill-line
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^I' complete-word # complete on tab, leave expansion to _expand
bindkey -r '^j' #unbind ctrl-j, I hit it all the time accidentaly
bindkey -r '^[x' # remove M-x for emacs-things


## Sourcing OS-specific things
[[ -f ~/.zsh.d/zsh.${OS} ]] && source ~/.zsh.d/zsh.${OS}

# Source z.sh if available
if [ -s ~/bin/z.sh ] ; then
    source ~/bin/z.sh ;
fi

# Use zsh syntax highlighting if available
if [ -s ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ] ; then
    source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# Source ~/.zsh.d/*
setopt EXTENDED_GLOB
for zshrc in ~/.zsh.d/[0-9][0-9]*[^~] ; do
    if [[ ! -z $ZSHDEBUG ]]; then
        echo +++ $(basename $zshrc)
    fi
    source $zshrc
done
unsetopt EXTENDED_GLOB

## GPG
# brew install gpg gpg-agent keychain
which keychain 2>&1 > /dev/null
if [[ $? -eq 0 ]]; then
    eval `keychain -q --eval --agents gpg,ssh --inherit any id_rsa`
fi

end=$EPOCHREALTIME

printf "+++Loaded files in %0.4f seconds\n" $(($end-$start))
