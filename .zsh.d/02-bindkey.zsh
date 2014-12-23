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
