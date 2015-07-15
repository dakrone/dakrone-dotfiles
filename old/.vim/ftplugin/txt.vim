" Ctrl-J for paragraph justification
:nmap <C-J> vipgq

" autoinsert data on <Ydt>
:iab <Ydt>  <C-R>=strftime("%a %d %b %Y %T %Z")<CR>

" spell checking
":source ~/.vim/plugin/vimspell.vim
":setlocal spell spelllang=en_ca,en_us,en
:setlocal spell spelllang=en

" screen width
:set textwidth=72
