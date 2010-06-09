" Vim script
" Maintainer: Peter Odding <peter@peterodding.com>
" Last Change: June 6, 2010
" URL: http://peterodding.com/code/vim/profile/autoload/xolox.vim

" Miscellaneous functions used throughout my Vim profile and plug-ins.

function! xolox#trim(s) " -- trim whitespace from start/end of string {{{1
  return substitute(a:s, '^\s*\(.\{-}\)\s*$', '\1', '')
endfunction

function! xolox#unique(list) " -- remove duplicate values from list (in-place) {{{1
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
			echomsg call('printf', a:args)
		elseif nargs == 1
			echomsg a:args[0]
		endif
	finally
    " Always clear message highlighting -- even when interrupted by Ctrl-C.
		echohl none
	endtry
endfunction
