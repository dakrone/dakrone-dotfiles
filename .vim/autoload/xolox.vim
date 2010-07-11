" Vim script
" Maintainer: Peter Odding <peter@peterodding.com>
" Last Change: July 5, 2010
" URL: http://peterodding.com/code/vim/profile/autoload/xolox.vim

" Miscellaneous functions used throughout my Vim profile and plug-ins.

" Lately I've been losing my message history a lot so I've added this option
" which keeps a ring buffer with the last N messages in "g:xolox_messages".
if !exists('g:xolox_message_buffer')
  let g:xolox_message_buffer = 100
endif

if !exists('g:xolox_messages')
  let g:xolox_messages = []
endif

function! xolox#trim(s) " -- trim whitespace from start/end of {s} {{{1
  return substitute(a:s, '^\s*\(.\{-}\)\s*$', '\1', '')
endfunction

function! xolox#quote_pattern(s) " -- convert {s} to pattern that matches {s} literally (on word boundaries!) {{{1
  let patt = xolox#escape#pattern(a:s)
  if patt =~ '^\w'
    let patt = '\<' . patt
  endif
  if patt =~ '\w$'
    let patt = patt . '\>'
  endif
  return patt
endfunction

function! xolox#unique(list) " -- remove duplicate values from {list} (in-place) {{{1
	let index = 0
	while index < len(a:list)
		let value = a:list[index]
		let match = index(a:list, value, index+1)
		if match >= 0
			call remove(a:list, match)
		else
			let index += 1
		endif
		unlet value
	endwhile
	return a:list
endfunction

function! xolox#message(...) " -- show a formatted informational message to the user {{{1
	return s:message('title', a:000)
endfunction

function! xolox#warning(...) " -- show a formatted warning message to the user {{{1
	return s:message('warningmsg', a:000)
endfunction

function! s:message(hlgroup, args) " -- implementation of message() and warning() {{{1
	try
		execute 'echohl' a:hlgroup
    " Redraw to avoid the |hit-enter| prompt.
		redraw
    let nargs = len(a:args)
		if nargs >= 2
      let message = call('printf', a:args)
		elseif nargs == 1
			let message = a:args[0]
		endif
    if exists('message')
      echomsg message
      if g:xolox_message_buffer > 0
        call add(g:xolox_messages, message)
        if len(g:xolox_messages) > g:xolox_message_buffer
          call remove(g:xolox_messages, 0)
        endif
      endif
    endif
	finally
    " Always clear message highlighting -- even when interrupted by Ctrl-C.
		echohl none
	endtry
endfunction
