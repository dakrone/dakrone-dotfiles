" Vim script
" Maintainer: Peter Odding <peter@peterodding.com>
" Last Change: June 8, 2010
" URL: http://peterodding.com/code/vim/profile/autoload/xolox/timer.vim

if !exists('g:timer_enabled')
  let g:timer_enabled = 0
endif

if !exists('g:timer_verbosity')
  let g:timer_verbosity = 1
endif

function! xolox#timer#start()
  if g:timer_enabled || &verbose >= g:timer_verbosity
    return reltime()
  endif
  return []
endfunction

function! xolox#timer#stop(start, message)
  if (g:timer_enabled || &verbose >= g:timer_verbosity) && a:start != []
    let duration = xolox#trim(reltimestr(reltime(a:start)))
    call xolox#message(a:message, duration)
  endif
endfunction

" vim: ts=2 sw=2 et
