export SONIAN_DIR=~/safe

# takes out the date because I know when today is
alias -g safetail="tail -F safe.log | sed 's/^20..-............................//;;s/\| \[main\] \|//g'"

