" process only once
if exists("b:did_ftplugin_c") || &compatible
   finish
endif
let b:did_ftplugin_c = 1

setlocal spell                  " enable spell check

" useful for arranging a block of comments
nnoremap <buffer> <C-J> vip=

" Setup tabstops to 6 and indents to 6
setlocal tabstop=6
setlocal shiftwidth=6
setlocal noexpandtab

" line wrapping in c comments not c code
setlocal textwidth=78           " Set the line wrap length


" c formatting options
let c_no_curly_error=1          " don't highlight {}; inside [] and () as errors
let c_no_c99=1
unlet c_no_c99                  " don't highlight C99 standard items


" C-mode formatting options
"   t auto-wrap comment
"   c allows textwidth to work on comments
"   q allows use of gq* for auto formatting
"   l don't break long lines in insert mode
"   r insert '*' on <cr>
"   o insert '*' on newline with 'o'
"   n recognize numbered bullets in comments
setlocal formatoptions=tcqlron


" C-mode options (cinoptions==cino)
" N	number of spaces
" Ns	number of spaces * shiftwidth
" >N	default indent
" eN	extra indent if the { is at the end of a line
" nN	extra indent if there is no {} block
" fN	indent of the { of a function block
" gN    indent of the C++ class scope declarations (public, private, protected)
" {N	indent of a { on a new line after an if,while,for...
" }N	indent of a } in reference to a {
" ^N	extra indent inside a function {}
" :N	indent of case labels
" =N	indent of case body
" lN	align case {} on the case line
" tN	indent of function return type
" +N	indent continued algibreic expressions
" cN	indent of comment line after /*
" )N	vim searches for closing )'s at most N lines away
" *N	vim searches for closing */ at most N lines away
setlocal cinoptions=:0l1t0g0

"      " funky BK macros...
"      ":noremap <buffer> <c-e> :!bk edit %<CR>:e!<CR>

" folding
"  - reserve 4 columns on the left for folding tree
"  - fold by syntax, use {}'s
"  - start with all folds open
if winwidth(0) > 80
    set foldcolumn=4
endif

fun! s:CSyntax()
        setlocal fdm=syntax
        %foldopen!
endfunction
"setlocal fdm=syntax
"syn region myFold start="{" end="}" transparent fold
au! FileReadPost,BufReadPost *.c call s:CSyntax()
"  - bind meta< and meta> for fold colapse and extend
"noremap <buffer> <m-,> :foldclose<CR>
"noremap <buffer> <m-.> :foldopen<CR>

" cool C abbreviations...
abb #i #include
abb #d #define

"source ~/.vim/ccommenter.vim

"source ~/.vim/vdb/vdb.vim

"source ~/.vim/localized_vimrc.vim

if has('gui_running')
        let winnum = bufwinnr("__Tag_List__")
        if winnum == -1
                "Tlist
        endif
endif

" ---------------------------------------------------------------------------
" next block is from chapter 10 of the windows.txt vim help file 
" :h preview-window
"
":au! CursorHold *.[ch] nested call PreviewWord()

nnoremap <buffer> <LocalLeader>pt :call PreviewWord()<cr>
func! PreviewWord()
        if &previewwindow                  " don't do this in the preview window
                return
        endif
        let w = expand("<cword>")          " get the word under cursor
        if w =~ '\a'                       " if the word contains a letter

                " Delete any existing highlight before showing another tag
                silent! wincmd P           " jump to preview window
                if &previewwindow          " if we really get there...
                        match none         " delete existing highlight
                        wincmd p           " back to old window
                endif

                " Try displaying a matching tag for the word under the cursor
                try
                        exe "ptag! " . w
                catch
                        return
                endtry

                silent! wincmd P           " jump to preview window
                if &previewwindow          " if we really get there...
                        if has("folding")
                                silent! .foldopen       " don't want a closed fold
                        endif
                        call search("$", "b")           " to end of previous line
                        let w = substitute(w, '\\', '\\\\', "")
                        call search('\<\V' . w . '\>')  " position cursor on match
                        " Add a match highlight to the word at this position
                        hi previewWord term=bold ctermbg=green guibg=green
                        exe 'match previewWord "\%' . line(".") . 'l\%' . col(".") . 'c\k*"'
                        wincmd p                        " back to old window
                endif
        endif
endfun

" ---------------------------------------------------------------------------
" This goes up to the previous function even if the { comes after the
" function name

function! FindFunctionDefinition(dir)
        let l:lastpattern = @/
        if a:dir == -1
                ?^\(\a.*(\_[^)]*) *\)\{,1\}{
        elseif a:dir == 1
                /^\(\a.*(\_[^)]*) *\)\{,1\}{
        endif
        let @/ = l:lastpattern
endfunction

nnoremap <buffer> [[ :call FindFunctionDefinition(-1)<CR>
nnoremap <buffer> ]] :call FindFunctionDefinition(1)<CR>

" not quite finished yet
function! FindPreviousFunctionEnd()
        let lastpattern = @/

        let foo = search ( '^\(\a.*(\_[^\)]*) *\)\{,1\}{', 'be' )
        exec '%'

        @/ = lastpattern
endfunction

