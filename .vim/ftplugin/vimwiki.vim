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
  setl formatlistpat=^\\s\\+[*#]\\s*
else
  setl comments=n:*\ [\ ],n:*[\ ],n:*\ [],n:*[],n:*\ [x],n:*[x]
  setl comments+=n:#\ [\ ],n:#[\ ],n:#\ [],n:#[],n:#\ [x],n:#[x]
  setl comments+=n:*,n:#
endif
setlocal formatoptions=tnro
" COMMENTS }}}

" FOLDING for headers and list items using expr fold method. {{{
if g:vimwiki_folding == 1
  setlocal fdm=expr
endif
setlocal foldexpr=VimwikiFoldLevel(v:lnum)

function! VimwikiFoldLevel(lnum) "{{{
  let line = getline(a:lnum)

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
  if g:vimwiki_fold_lists
    let rx_list_item = '\('.
          \ g:vimwiki_rxListBullet.'\|'.g:vimwiki_rxListNumber.
          \ '\)'

    if line =~ rx_list_item
      let [nnum, nline] = s:find_next_item(rx_list_item, a:lnum)
      if nline =~ rx_list_item
        let level = s:get_li_level(a:lnum, nnum)
        if !(level < 0 && (nnum - a:lnum) > 1)
          return s:fold_marker(level)
        endif
      elseif nnum - a:lnum == 1
        " last single-lined list item in a list
        let level = s:get_li_level_last(a:lnum)
        return s:fold_marker(level)
      endif
    else
      let [pnum, pline] = s:find_prev_item(rx_list_item, a:lnum)
      if pline =~ rx_list_item
        if getline(a:lnum + 1) =~ rx_list_item
          let level = s:get_li_level(pnum, a:lnum + 1)
          if level < 0
            return s:fold_marker(level)
          endif
        endif

        let [nnum, nline] = s:find_next_item(rx_list_item, pnum)
        if nline !~ rx_list_item && nnum-a:lnum == 1
          " last multi-lined list item in a list
          let level = s:get_li_level_last(pnum)
          return s:fold_marker(level)
        endif

      endif
    endif

  endif

  return '='
endfunction "}}}

function! s:fold_marker(level) "{{{
  if a:level > 0
    return "a".a:level
  elseif a:level < 0
    return "s".abs(a:level)
  else
    return "="
  endif
endfunction "}}}

function! s:find_next_item(rx_item, lnum) "{{{
  let lnum = a:lnum + 1

  while lnum <= line('$')
    if getline(lnum) =~ a:rx_item
          \ || getline(lnum) =~ '^\S'
          \ || indent(lnum) <= indent(a:lnum)
      break
    endif
    let lnum += 1
  endwhile

  return [lnum, getline(lnum)]
endfunction "}}}

function! s:find_prev_item(rx_item, lnum) "{{{
  let lnum = a:lnum - 1

  while lnum > 1
    if getline(lnum) =~ a:rx_item
          \ || getline(lnum) =~ '^\S'
      break
    endif
    let lnum -= 1
  endwhile

  return [lnum, getline(lnum)]
endfunction "}}}

function! s:get_li_level(lnum, nnum) "{{{
  if VimwikiGet('syntax') == 'media'
    let level = vimwiki#count_first_sym(getline(a:nnum)) -
          \ vimwiki#count_first_sym(getline(a:lnum))
  else
    let level = ((indent(a:nnum) - indent(a:lnum)) / &sw)
  endif
  return level
endfunction "}}}

function! s:get_li_level_last(lnum) "{{{
  if VimwikiGet('syntax') == 'media'
    return -(vimwiki#count_first_sym(getline(a:lnum)) - 1)
  else
    return -(indent(a:lnum) / &sw - 1)
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
  if has("unix")
    nmap <silent><buffer> <C-@> <Plug>VimwikiToggleListItem
  endif
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
