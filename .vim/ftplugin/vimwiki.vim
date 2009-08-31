" Vimwiki filetype plugin file
" Author: Maxim Kim <habamax@gmail.com>
" Home: http://code.google.com/p/vimwiki/

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1  " Don't load another plugin for this buffer

" UNDO list {{{
" Reset the following options to undo this plugin.
let b:undo_ftplugin = "setlocal wrap< linebreak< ".
      \ "suffixesadd< isfname< comments< ".
      \ "autowriteall< ".
      \ "formatoptions< foldtext< ".
      \ "foldmethod< foldexpr< commentstring< "
" UNDO }}}

" MISC STUFF {{{

setlocal wrap
setlocal linebreak
setlocal autowriteall
setlocal commentstring=<!--%s-->
" MISC }}}

" GOTO FILE: gf {{{
execute 'setlocal suffixesadd='.VimwikiGet('ext')
setlocal isfname-=[,]
" gf}}}

" COMMENTS: autocreate list items {{{
" for list items, and list items with checkboxes
if VimwikiGet('syntax') == 'default'
  setl comments=b:\ *\ [\ ],b:\ *[\ ],b:\ *\ [],b:\ *[],b:\ *\ [x],b:\ *[x]
  setl comments+=b:\ #\ [\ ],b:\ #[\ ],b:\ #\ [],b:\ #[],b:\ #\ [x],b:\ #[x]
  setl comments+=b:\ *,b:\ #
else
  setl comments=n:*\ [\ ],n:*[\ ],n:*\ [],n:*[],n:*\ [x],n:*[x]
  setl comments+=n:#\ [\ ],n:#[\ ],n:#\ [],n:#[],n:#\ [x],n:#[x]
  setl comments+=n:*,n:#
endif
setlocal formatoptions=ctnqro
" COMMENTS }}}

" FOLDING for headers and list items using expr fold method. {{{
if VimwikiGet('folding')
  setlocal fdm=expr
endif
setlocal foldexpr=VimwikiFoldLevel(v:lnum)
function! VimwikiFoldLevel(lnum) "{{{
  let line = getline(a:lnum)
  let nline = getline(a:lnum + 1)

  " Header folding...
  if line =~ g:vimwiki_rxHeader
    let n = vimwiki#count_first_sym(line)
    return '>' . n
  endif

  if g:vimwiki_fold_empty_lines == 0
    let nnline = getline(nextnonblank(a:lnum + 1))
    if nnline =~ g:vimwiki_rxHeader
      let n = vimwiki#count_first_sym(nnline)
      return '<' . n
    endif
  endif

  " List item folding...
  let nnum = a:lnum + 1

  let rx_list_item = '\('.
        \ g:vimwiki_rxListBullet.'\|'.g:vimwiki_rxListNumber.
        \ '\)'
  if line =~ rx_list_item && nline =~ rx_list_item
    return s:get_li_level(a:lnum, nnum)
  " list is over, remove foldlevel
  elseif line =~ rx_list_item && nline !~ rx_list_item
    return s:get_li_level_last(a:lnum)
  endif

  return '='
endfunction "}}}

function! s:get_li_level(lnum, nnum) "{{{
  if VimwikiGet('syntax') == 'media'
    let level = s:count_first_sym(getline(a:nnum)) -
          \ s:count_first_sym(getline(a:lnum))
    if level > 0
      return "a".level
    elseif level < 0
      return "s".abs(level)
    else
      return "="
    endif
  else
    let level = ((indent(a:nnum) - indent(a:lnum)) / &sw)
    if level > 0
      return "a".level
    elseif level < 0
      return "s".abs(level)
    else
      return "="
    endif
  endif
endfunction "}}}

function! s:get_li_level_last(lnum) "{{{
  if VimwikiGet('syntax') == 'media'
    return "s".(s:count_first_sym(getline(a:lnum)) - 1)
  else
    return "s".(indent(a:lnum) / &sw - 1)
  endif
endfunction "}}}

setlocal foldtext=VimwikiFoldText()
function! VimwikiFoldText() "{{{
  let line = getline(v:foldstart)
  return line.' ['.(v:foldend - v:foldstart).'] '
endfunction "}}}

" FOLDING }}}

" COMMANDS {{{
command! -buffer Vimwiki2HTML
      \ call vimwiki_html#Wiki2HTML(expand(VimwikiGet('path_html')),
      \                             expand('%'))
command! -buffer VimwikiAll2HTML
      \ call vimwiki_html#WikiAll2HTML(expand(VimwikiGet('path_html')))

command! -buffer VimwikiNextWord call vimwiki#WikiNextWord()
command! -buffer VimwikiPrevWord call vimwiki#WikiPrevWord()
command! -buffer VimwikiDeleteWord call vimwiki#WikiDeleteWord()
command! -buffer VimwikiRenameWord call vimwiki#WikiRenameWord()
command! -buffer VimwikiFollowWord call vimwiki#WikiFollowWord('nosplit')
command! -buffer VimwikiGoBackWord call vimwiki#WikiGoBackWord()
command! -buffer VimwikiSplitWord call vimwiki#WikiFollowWord('split')
command! -buffer VimwikiVSplitWord call vimwiki#WikiFollowWord('vsplit')

command! -buffer VimwikiToggleListItem call vimwiki_lst#ToggleListItem()
" COMMANDS }}}

" KEYBINDINGS {{{
if g:vimwiki_use_mouse
  nmap <buffer> <S-LeftMouse> <NOP>
  nmap <buffer> <C-LeftMouse> <NOP>
  noremap <silent><buffer> <2-LeftMouse> :VimwikiFollowWord<CR>
  noremap <silent><buffer> <S-2-LeftMouse> <LeftMouse>:VimwikiSplitWord<CR>
  noremap <silent><buffer> <C-2-LeftMouse> <LeftMouse>:VimwikiVSplitWord<CR>
  noremap <silent><buffer> <RightMouse><LeftMouse> :VimwikiGoBackWord<CR>
endif

if !hasmapto('<Plug>VimwikiFollowWord')
  nmap <silent><buffer> <CR> <Plug>VimwikiFollowWord
endif
noremap <silent><script><buffer>
      \ <Plug>VimwikiFollowWord :VimwikiFollowWord<CR>

if !hasmapto('<Plug>VimwikiSplitWord')
  nmap <silent><buffer> <S-CR> <Plug>VimwikiSplitWord
endif
noremap <silent><script><buffer>
      \ <Plug>VimwikiSplitWord :VimwikiSplitWord<CR>

if !hasmapto('<Plug>VimwikiVSplitWord')
  nmap <silent><buffer> <C-CR> <Plug>VimwikiVSplitWord
endif
noremap <silent><script><buffer>
      \ <Plug>VimwikiVSplitWord :VimwikiVSplitWord<CR>

if !hasmapto('<Plug>VimwikiGoBackWord')
  nmap <silent><buffer> <BS> <Plug>VimwikiGoBackWord
endif
noremap <silent><script><buffer>
      \ <Plug>VimwikiGoBackWord :VimwikiGoBackWord<CR>

if !hasmapto('<Plug>VimwikiNextWord')
  nmap <silent><buffer> <TAB> <Plug>VimwikiNextWord
endif
noremap <silent><script><buffer>
      \ <Plug>VimwikiNextWord :VimwikiNextWord<CR>

if !hasmapto('<Plug>VimwikiPrevWord')
  nmap <silent><buffer> <S-TAB> <Plug>VimwikiPrevWord
endif
noremap <silent><script><buffer>
      \ <Plug>VimwikiPrevWord :VimwikiPrevWord<CR>

if !hasmapto('<Plug>VimwikiDeleteWord')
  nmap <silent><buffer> <Leader>wd <Plug>VimwikiDeleteWord
endif
noremap <silent><script><buffer>
      \ <Plug>VimwikiDeleteWord :VimwikiDeleteWord<CR>

if !hasmapto('<Plug>VimwikiRenameWord')
  nmap <silent><buffer> <Leader>wr <Plug>VimwikiRenameWord
endif
noremap <silent><script><buffer>
      \ <Plug>VimwikiRenameWord :VimwikiRenameWord<CR>

if !hasmapto('<Plug>VimwikiToggleListItem')
  nmap <silent><buffer> <C-Space> <Plug>VimwikiToggleListItem
endif
noremap <silent><script><buffer>
      \ <Plug>VimwikiToggleListItem :VimwikiToggleListItem<CR>

" Text objects {{{
omap <silent><buffer> ah :<C-U>call vimwiki#TO_header(0)<CR>
vmap <silent><buffer> ah :<C-U>call vimwiki#TO_header(0)<CR>

omap <silent><buffer> ih :<C-U>call vimwiki#TO_header(1)<CR>
vmap <silent><buffer> ih :<C-U>call vimwiki#TO_header(1)<CR>

nmap <silent><buffer> = :call vimwiki#AddHeaderLevel()<CR>
nmap <silent><buffer> - :call vimwiki#RemoveHeaderLevel()<CR>

" }}}

" KEYBINDINGS }}}
