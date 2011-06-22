if [ "$(whoami)" = "root" ]
then CARETCOLOR="red"
else CARETCOLOR="green"
fi

# ignore Xanadu as a hostname
if [ "$(hostname)" = "Xanadu.local" ]
then HN=""
else HN="$(hostname)"
fi


local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"

# Handle dumb (emacs) terminals with no color
if [ $TERM = "dumb" ]
then PROMPT='‹ %~ › ∴ '
else PROMPT='%{$fg[grey]%}‹ %{${fg[blue]}%}%~ $(git_prompt_info)%{$fg[grey]%}› %{${fg[$CARETCOLOR]}%}∴%{${reset_color}%} '
fi

RPS1='${return_code} $HN'

# hostname
#${fg[grey]}%}%m%{${fg[grey]}%}
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[green]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}⚡%{$reset_color%}"
