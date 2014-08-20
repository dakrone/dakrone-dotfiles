#!/bin/zsh

autoload colors
colors

versionize() {
    local file=$1
    local ver=$(git log --pretty=format:"%ai %h" -1 | perl -pe 's,(\d\d\d\d)-(\d\d)-(\d\d) (\d\d):(\d\d):(\d\d) [^ ]+ ([a-z0-9]+),\1\2\3\4\5\6+\7,')
    local versionedfile=$(echo $file | perl -pe "s,(.*)\.([^.]+)\$,\$1-${ver}.\$2,")
    cp $file $versionedfile
}

git_branch() {
    git branch --no-color 2>/dev/null | grep '^*' | colrm 1 2
    # $pipestatus[1] for the git exit code
}

if not_in_cloud; then
    autoload -Uz vcs_info

    if [[ ! $TERM = "dumb" ]]; then
        zstyle ":vcs_info:*" check-for-changes true
        zstyle ":vcs_info:*" stagedstr "%F{green}●"
        zstyle ":vcs_info:*" unstagedstr "%F{yellow}●"
        zstyle ":vcs_info:(sv[nk]|bzr):*" branchformat "%b%F{1}:%F{yellow}%r%{$reset_color%}"
        zstyle ":vcs_info:*" enable git svn bzr hg
        precmd () {
            if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
                   zstyle ":vcs_info:*" formats "%b%c%u%{$reset_color%}"
               } else {
                   zstyle ":vcs_info:*" formats "%b%c%u%F{red}●%{$reset_color%}"
               }
               vcs_info
        }
    else
        zstyle ":vcs_info:*" check-for-changes true
        zstyle ":vcs_info:*" stagedstr "●"
        zstyle ":vcs_info:*" unstagedstr "●"
        zstyle ":vcs_info:(sv[nk]|bzr):*" branchformat "%b:%r"
        zstyle ":vcs_info:*" enable git svn bzr hg
        precmd () {
            if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
                   zstyle ":vcs_info:*" formats "%b%c%u"
               } else {
                   zstyle ":vcs_info:*" formats "%b%c%u●"
               }
               vcs_info
        }
    fi
fi
