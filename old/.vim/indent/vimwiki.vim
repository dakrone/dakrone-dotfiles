" Vimwiki indent file
" Author: Maxim Kim <habamax@gmail.com>
" Home: http://code.google.com/p/vimwiki/

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

" Some preliminary settings
setlocal nolisp		" Make sure lisp indenting doesn't supersede us
setlocal autoindent	" indentexpr isn't much help otherwise

setlocal indentexpr=GetVimwikiIndent(v:lnum)
setlocal indentkeys+=<:>

" Only define the function once.
if exists("*GetVimwikiIndent")
  finish
endif

" Come here when loading the script the first time.

function GetVimwikiIndent(lnum)
  " Search backwards for the previous non-empty line.
  let plnum = prevnonblank(v:lnum - 1)
  if plnum == 0
    " This is the first non-empty line, use zero indent.
    return 0
  endif

  " TODO: use g:vimwiki_rxList here
  let lst_indent = len(matchstr(getline(a:lnum), '^\s\+\ze\(\*\|#\)'))
  if lst_indent > 0
    if lst_indent < &sw
      return &sw
    endif

    if has("float")
      let mul = round(lst_indent * 1.0 / &sw)
      let ind = float2nr(mul * &sw)
    else
      let mul = lst_indent / &sw
      let ind = mul * &sw
    endif

    return ind
  endif


  return -1
endfunction

" vim:sw=2
