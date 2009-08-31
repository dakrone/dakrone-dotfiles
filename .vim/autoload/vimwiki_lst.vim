" Vimwiki autoload plugin file
" Todo lists related stuff here.
" Author: Maxim Kim <habamax@gmail.com>
" Home: http://code.google.com/p/vimwiki/

if exists("g:loaded_vimwiki_list_auto") || &cp
  finish
endif
let g:loaded_vimwiki_lst_auto = 1

" Script variables {{{
" used in various checks
let s:rx_list_item = '\('.
      \ g:vimwiki_rxListBullet.'\|'.g:vimwiki_rxListNumber.
      \ '\)'
let s:rx_cb_list_item = s:rx_list_item.'\s*\zs\[.\?\]'
let s:rx_li_box = '\[.\?\]'
let s:rx_li_unchecked = '\[\s\?\]'
" used in substitutions
let s:rx_li_check = '\[x\]'
let s:rx_li_uncheck = '\[ \]'
" }}}

" Script functions {{{
" Set state of the list item on line number "lnum" to [ ] or [x]
function! s:set_state(lnum, on_off)"{{{
  let line = getline(a:lnum)
  if a:on_off
    let state = s:rx_li_check
  else
    let state = s:rx_li_uncheck
  endif
  let line = substitute(line, s:rx_li_box, state, '')
  call setline(a:lnum, line)
endfunction"}}}

" Get state of the list item on line number "lnum"
function! s:get_state(lnum)"{{{
  let state = 1
  let line = getline(a:lnum)
  let opt = matchstr(line, s:rx_cb_list_item)
  if opt =~ s:rx_li_unchecked
    let state = 0
  endif
  return state
endfunction"}}}

" Returns 1 if line is list item, 0 otherwise
function! s:is_cb_list_item(lnum)"{{{
  return getline(a:lnum) =~ s:rx_cb_list_item
endfunction"}}}

" Returns 1 if line is list item, 0 otherwise
function! s:is_list_item(lnum)"{{{
  return getline(a:lnum) =~ s:rx_list_item
endfunction"}}}

" Returns char column of checkbox. Used in parent/child checks.
function! s:get_li_pos(lnum) "{{{
  return stridx(getline(a:lnum), '[')
endfunction "}}}

" Returns list of line numbers of parent and all its child items.
function! s:get_child_items(lnum)"{{{
  let result = []
  let lnum = a:lnum
  let parent_pos = s:get_li_pos(lnum)

  " add parent
  call add(result, lnum)
  let lnum += 1

  while s:is_cb_list_item(lnum) &&
        \ s:get_li_pos(lnum) > parent_pos &&
        \ lnum <= line('$')

    call add(result, lnum)
    let lnum += 1
  endwhile

  return result
endfunction"}}}

" Returns list of line numbers of all items of the same level.
function! s:get_sibling_items(lnum)"{{{
  let result = []
  let lnum = a:lnum
  let ind = s:get_li_pos(lnum)

  while s:is_cb_list_item(lnum) &&
        \ s:get_li_pos(lnum) >= ind &&
        \ lnum <= line('$')

    if s:get_li_pos(lnum) == ind
      call add(result, lnum)
    endif
    let lnum += 1
  endwhile

  let lnum = a:lnum - 1
  while s:is_cb_list_item(lnum) &&
        \ s:get_li_pos(lnum) >= ind &&
        \ lnum >= 0

    if s:get_li_pos(lnum) == ind
      call add(result, lnum)
    endif
    let lnum -= 1
  endwhile

  return result
endfunction"}}}

" Returns line number of the parent of lnum item
function! s:get_parent_item(lnum)"{{{
  let lnum = a:lnum
  let ind = s:get_li_pos(lnum)

  while s:is_cb_list_item(lnum) &&
        \ s:get_li_pos(lnum) >= ind &&
        \ lnum >= 0
    let lnum -= 1
  endwhile

  if s:is_cb_list_item(lnum)
    return lnum
  else
    return a:lnum
  endif
endfunction"}}}

" Creates checkbox in a list item.
function s:create_cb_list_item(lnum) "{{{
  let line = getline(a:lnum)
  let m = matchstr(line, s:rx_list_item)
  if m != ''
    let line = m.' [ ]'.strpart(line, len(m))
    call setline(a:lnum, line)
  endif
endfunction "}}}

" Script functions }}}

" Toggle list item between [ ] and [x]
function! vimwiki_lst#ToggleListItem()"{{{
  let current_lnum = line('.')

  if !s:is_cb_list_item(current_lnum)
    if g:vimwiki_auto_checkbox
      call s:create_cb_list_item(current_lnum)
    endif
    return
  endif

  let current_state = s:get_state(current_lnum)
  if  current_state == 0
    for lnum in s:get_child_items(current_lnum)
      call s:set_state(lnum, 1)
      let new_state = 1
    endfor
  else
    for lnum in s:get_child_items(current_lnum)
      call s:set_state(lnum, 0)
      let new_state = 0
    endfor
  endif

  let c_lnum = current_lnum
  while s:is_cb_list_item(c_lnum)
    let all_items_checked = 1
    for lnum in s:get_sibling_items(c_lnum)
      if s:get_state(lnum) != 1
        let all_items_checked = 0
        break
      endif
    endfor

    let parent_lnum = s:get_parent_item(c_lnum)
    if parent_lnum == c_lnum
      break
    endif
    call s:set_state(parent_lnum, all_items_checked)


    let c_lnum = parent_lnum
  endwhile
endfunction"}}}
