"{{{ Ruby block delimiter conversion: do end <=> { }
"Copyright © 2005 Mauricio Fernandez
"Subject to same licensing terms as Ruby.
" requires matchit and friends 
" since it uses the % and = bindings
function! s:String_Strip(str)
    let s = substitute(a:str, '\v^\s*', '', '')
    return substitute(s, '\v\s*$', '', '')
endfunction

function! s:RubyBlockBraceToDoEnd(lineno)
    " { } => do end
    let oldz = getreg("z")
    call setreg("z", "")
    execute 'normal ^f{%l"zd$'
    let suffix = s:String_Strip(getreg("z"))
    call setreg("z", oldz)
    let orig = getline(".")
    let repl = substitute(orig, '\v\s*\{\s*(\|[^|]*\|)?.*', ' do \1', '')
    call setline(".", repl)
    let nextline = substitute(orig, '\v[^{]*\v\s*\{\s*(\|[^|]*\|)?', '', '')
    let nextline = substitute(nextline, '\}[^}]*$', '', '')
    let numlines = 0
    

    " uncomment one of the following:
    
    " (1) just insert the body without splitting the lines on ;
    "call append(a:lineno, nextline)
    "call append(a:lineno+1, 'end' . suffix)
    "
    
    " (2) try to split on ; ...
    call append(a:lineno, 'end' . suffix)
    " this is what we would want to do: 
    "let nextline = substitute(nextline, ';', "\n", 'g')
    
    while stridx(nextline, ";") != -1
	let eom = stridx(nextline, ";")
	let line = s:String_Strip(strpart(nextline, 0, eom))
	call append(a:lineno + numlines, line) 
	let numlines = numlines + 1
	let nextline = strpart(nextline, eom+1, strlen(nextline) - eom - 1)
    endwhile
    let nextline = s:String_Strip(nextline)
    if strlen(nextline) > 0
	call append(a:lineno + numlines, nextline)
	let numlines = numlines + 1
    endif

    " this is what it all began with...
    "execute 'normal :s/\v\s*\{\s*(\|.*\|)?/ do \1\r/'
    "execute 'normal g_cwend'
    execute 'normal V' . (1 + numlines) . 'j='
    "echo "{ } => do end"
endfunction

function! s:RubyBlockDoEndToBrace(_firstline, _lastline)
    " do end => { }
    let linenum = a:_firstline + 1
    let orig = getline(".")
    while linenum < a:_lastline - 1
	let addline = getline(linenum)
	if '\v^\s*$' !~ addline
	    let addline = substitute(addline, '\v^\s*', '', '')
	    let addline = substitute(addline, '\s*$', '; ', '')
	    let orig = orig . addline
	endif
	let linenum = linenum + 1
    endwhile
    let l = substitute(getline(a:_lastline-1), '\v^\s*', '', '')
    let l = substitute(l, '\s*$', '', '')
    let orig = orig . l
    let l = substitute(getline(a:_lastline), '\v^\s*end(\.|\s|$)@=', ' }', '')
    let l = substitute(l, '\s*$', '', '')
    let orig = orig . l
    
    "echo orig
    "input(orig)
    let repl = substitute(orig, '\v\s*do\s*(\|[^|]*\|)?', '{\1 ', '')
    "execute 'normal d' . (a:_lastline - a:_firstline) . 'j'
    execute ':' . a:_firstline . ',' . a:_lastline . 'd'
    call append(a:_firstline - 1, repl)
    execute ':' . a:_firstline
    "echo "do end => { }"
endfunction

map <SID>xx <SID>xx
let s:sid = maparg("<SID>xx")
unmap <SID>xx
let s:sid = substitute(s:sid, 'xx', '', '')

function! <SID>RubyBlockSwitchDelimiters() range
    set nofoldenable
    if a:firstline == a:lastline
	let braceidx = match(getline("."), '{')
	let doidx = match(getline("."), '\<do\>')
	if braceidx != -1 && (doidx == -1 || braceidx < doidx)
	    call s:RubyBlockBraceToDoEnd(a:firstline)
	elseif doidx != -1
	    execute 'normal /\<do\>' . "\n" . 'V%:call ' . 
			\ s:sid . 'RubyBlockSwitchDelimiters()' . "\n"
	else
	    echo "No block found"
	end
    else
	call s:RubyBlockDoEndToBrace(a:firstline, a:lastline)
    endif
    "execute 'normal V2k='
    "execute 'normal v5j'
endfunction

command! -range B <line1>,<line2>call <SID>RubyBlockSwitchDelimiters()
vmap <Leader>B :call <SID>RubyBlockSwitchDelimiters()<cr>
