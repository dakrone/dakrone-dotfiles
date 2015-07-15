" Script Nmame: code toolkit
" File Name:    ctk.vim
" Author:       StarWing
" Version:      0.5
" Last Change:  2010-01-28 20:53:29
" Note:         see :ctk for details
" ======================================================{{{1

if v:version < 700
    echomsg "ctk.vim requires Vim 7.0 or above."
    finish
endif

let s:cpo_save = &cpo
set cpo&vim

scriptencoding utf-8

if !exists('g:loaded_ctk')
    " =================================================={{{2
    let g:loaded_ctk = 'v0.4'

    " options {{{2
    function! s:defopt(opt, val)
	if !exists(a:opt) | let {a:opt} = a:val | endif
    endfunction

    call s:defopt('g:ctk_autofname', 'strftime("%Y-%m-%d")."-".idx')
    call s:defopt('g:ctk_autostart', 1)
    call s:defopt('g:ctk_cinfo_file', '.compiler_info')
    call s:defopt('g:ctk_defoutput', './output')
    call s:defopt('g:ctk_ext_var', 'ft_ext')
    call s:defopt('g:ctk_tempdir', './noname')

    if has('win32')
        call s:defopt('g:ctk_cmdenc', 'cp936')
        call s:defopt('g:ctk_envvarfmt', '%var%')
        call s:defopt('g:ctk_execprg', executable('vimrun') ?
                    \ 'start vimrun $exec' : 'start $exec')
    elseif has('unix')
        call s:defopt('g:ctk_cmdenc', 'utf-8')
        call s:defopt('g:ctk_envvarfmt', '${var}')
        call s:defopt('g:ctk_execprg', has('gui_running') ? 'xterm -e "$exec; '.
                    \ 'echo \"$exec returned $?\";read -s -n1 '.
                    \ '-p\"press any key to continue...\"" &' : '')
    else
        call s:defopt('g:ctk_cmdenc', '')
        call s:defopt('g:ctk_envvarfmt', '')
        call s:defopt('g:ctk_execprg', '')
    endif

    delfunc s:defopt

    " commands & menus {{{2
    command! -bar -bang StartCTK call s:start_ctk('<bang>')
    command! -bar StopCTK call s:stop_ctk()
    command! -bar RefreshCTK call s:refresh_ctk()
    command! -bar EditCompilerInfo exec 'drop '.globpath(&rtp, g:ctk_cinfo_file)
                \| au BufWritePost <buffer> call s:refresh_ctk()
    
    command! -bar -nargs=1 SetExtensionName let b:{g:ctk_ext_var} = <q-args>
    command! -nargs=* -complete=custom,s:info_item_complete -bang
	    \ SetCompilerInfo call s:call('s:set_compiler_info', [<q-args>, '<bang>'])
    command! -nargs=+ -complete=custom,s:info_item_complete -bang
            \ SetDefaultInfo call s:call('s:set_default_info', [<q-args>, '<bang>'])

    command! -nargs=* -complete=customlist,s:info_name_complete -bar -count=0 
	    \ ListCompiler call s:find_and_call('s:list_compiler', [<q-args>, <count>])
    command! -nargs=+ -complete=custom,s:info_item_complete -count=0
	    \ AddFlags call s:find_and_call('s:add_flags', [<q-args>, <count>])
    command! -nargs=? -bar -bang -count=0 CC call s:find_and_call('s:compile',
                \ [<count>, <q-args>, <q-bang>])
    command! -nargs=? -bar -bang -count=0 RUN call s:find_and_call('s:run',
                \ [<count>, <q-args>, <q-bang>])

    amenu &Tools.&CTK.&Start :StartCTK<CR>
    amenu &Tools.&CTK.&Stop  :StopCTK<CR>
    amenu &Tools.&CTK.-Sep- :
    amenu <silent> &Tools.&CTK.&Add\ a\ modeline :exec 'AddFlags '.(has('gui_running') ? inputdialog("Please input the text in modeline:", "flags += ''") : input("Modeline:", "flags += ''"))<CR>
    amenu &Tools.&CTK.&List\ All\ Compiler :ListCompiler all<CR>
    amenu &Tools.&CTK.&Compile :CC<CR>
    amenu &Tools.&CTK.&Run :RUN<CR>

    " small functions {{{2
    let s:sfile = expand('<sfile>')

    function! s:start_ctk(bang) " {{{3
        augroup ctk_autocmds
            au!

            " 1 means this name is user-modifiled
            au BufFilePost * let b:ctk_fname = [expand('%:p'), &ft, 1]

            au FileType * call s:call('s:begin_setinfo', [])
            exec 'run '.g:ctk_cinfo_file
            au FileType * call s:call('s:end_setinfo', [])

            au FileType * call s:call('s:set_fname', [])
        augroup END

        map gc <C-\><C-N>:<C-U>exec v:count."CC!"<CR>
        map gC <C-\><C-N>:<C-U>exec v:count."RUN"<CR>

        if a:bang == '!'
            call s:call('s:delete_ci', [])
            filetype detect
        endif
    endfunction

    function! s:stop_ctk() " {{{3
        unlet! b:ctk_fname
        call s:delete_ci()
        silent! au! ctk_autocmds
        silent! unmap gc
        silent! unmap gC
    endfunction

function! s:refresh_ctk() " {{{3
    call s:start_ctk('')
    let cur_winnr = winnr()

    while 1
        if exists('b:compiler_info') 
            call s:delete_ci()
            silent! filetype detect
        endif
        silent! wincmd w
        if winnr() == cur_winnr | return | endif
    endwhile
endfunction

    function! s:echoerr(msg) " {{{3
        echohl ErrorMsg
        echomsg 'CTK: '.a:msg
        echohl NONE
    endfunction

    function! s:call(funcname, args) " {{{3
        if !exists('s:load_all')
            exec 'so '.s:sfile
            unlet s:sfile
        endif

        return call(a:funcname, a:args)
    endfunction

    function! s:find_and_call(funcname, args) " {{{3
        let cur_winnr = winnr()

        while 1
            if exists('b:compiler_info') 
                return s:call(a:funcname, a:args)
            endif
            silent! wincmd w
            if winnr() == cur_winnr | break | endif
        endwhile

        call s:echoerr("command can't use, because ctk isn't avaliable")
    endfunction

function! s:info_name_complete(A, L, P) " {{{3
    if !exists(s:ci_name) | return [] | endif
    let list = []
    for dict in b:{s:ci}.list
        let list += [dict.name]
    endfor
    let pat = "'^\\v".substitute(escape(a:A, '\'), "'", "''", 'g')."'"
    return sort(filter(list + ['all', 'default', 'current'],
                \ 'v:val =~ '.pat))
endfunction

function! s:info_item_complete(A, L, P) " {{{3
    return "asm_\ncc\ncmd\ndebug_\nflags\n".
                \ "input\noutput\nrun\ntitle"
endfunction " }}}3

    " }}}2
    " =================================================={{{2
    if g:ctk_autostart
        StartCTK
    else
        let &cpo = s:cpo_save
        unlet s:cpo_save

        finish
    endif " }}}2
endif

let s:load_all = 1

" some inner variables {{{1

" escape chars in filename {{{2
let s:fname_escape_chars = " \t\n*?[{`$\\%#'\"|!<"

" buffer-variable names {{{2
let s:ci = 'compiler_info'
let s:ci_name = 'b:'.s:ci

" some default attr {{{2
let s:def_attr = {'cmd': ':echo "Done Nothing"', 'run': ':echo "Done Nothing"',
            \ 'input': '%:.', 'output': '%:t:r'}

" patterns {{{2

let s:pat_cmd_is_shell = '\v^[^:]|^:!|^:sil%[ent]\s+!'
let s:pat_cmdtag = '\v\$(\h\w*)|\$\{([Qq]-)=(\h\w*)}'
let s:pat_com = ':\zs[^,]\+'
let s:pat_com_begin = 's.\=:\zs[^,]\+\ze'
let s:pat_com_end = 'e.\=:\zs[^,]\+\ze'
let s:pat_execoutput = '\%^[\r\n]*\zs.\{-}\ze[\r\n]*\%$'
let s:pat_exectag = '$exec\>'
let s:pat_filespec = '\v%(\%|#%(\d+)=|##|<cword>|<cWORD>)%(:[p8~.htre]|g=s(.).\{-}\1.\{-}\1)*'
let s:pat_filespec_escape = '\\\ze'.s:pat_filespec
let s:pat_filespec_nonescape = '\\\@<!'.s:pat_filespec
let s:pat_fname_escape = "[ \t\n*?[{`$\\%#''\"|!<]"
let s:pat_info ='\v^\s*(.{-})%(\s+(.{-})\s*)=$' 
let s:pat_info_var = '\v(\w+)\s*(\+)=\=\s*(\S)(.{-})\3'
let s:pat_modeline = '\v<cc%(-([^:]*))=:\s*(.*)'
let s:pat_run_direct = '#*RUN_DIRECT'
let s:pat_shellcmdtitle = '^!\=\zs.\{-}\ze\(\s\|$\)'

" }}}2

" ============================================================
" utility functions {{{1

" tricks to get command-style arglist {{{2
command! -nargs=* CTKGetEscapedList let l:args = [<f-args>]
function! s:get_escaped_list(str)
    exec 'CTKGetEscapedList '.a:str
    return args
endfunction

" command to unmap info {{{2
command! -nargs=1 CTKUnMapCompilerInfo
            \ exec join(map(<args>.unmap,
            \ 'v:val[0]."unmap ".v:val[1:]'), '|')

function! s:question(msg) " {{{2
    redraw
    echohl Question
    echo a:msg
    echohl NONE
    call inputsave()
    let ch = nr2char(getchar())
    call inputrestore()
    return ch
endfunction

function! s:get_idx(info) " {{{2
    let idx = 0
    for info in b:{s:ci}.list
        if info is a:info
            return idx
        endif
        let idx += 1
    endfor
    return -1
endfunction

function! s:get_entry_val(entry, key, default) " {{{2
    let default = get(b:{s:ci}, 'default', {})
    let info = get(get(b:{s:ci}, 'status', {}), 'info', {})
    let key = (a:entry == '' ? a:key : a:entry.'_'.a:key)

    return get(info, key,
                \ get(default, key, a:entry == '' ?
                \ get(s:def_attr, key, a:default) :
                \ get(info, a:key,
                \ get(default, a:key,
                \ get(s:def_attr, a:key, a:default)))))
endfunction

function! s:get_info(name) " {{{2
"    call Dfunc('s:get_info(name = '.a:name.')')
"    call Decho('info_list = '.string(b:{s:ci}.list))
    for info in b:{s:ci}.list
        if info.name ==? a:name
"            call Dret('s:get_info : '.string(info))
            return info
        endif
    endfor
"    call Dret('s:get_info : {}')
    return {}
endfunction

function! s:delete_ci() " {{{2
    if &ft ==? 'decho' | return | endif
"    call Dfunc('s:delete_ci()')
"    call Decho("delete b:compiler_info for buffer ".bufnr(""))

    unlet! b:{g:ctk_ext_var}
    if exists('b:compiler_info')
        for info in get(b:compiler_info, 'list', [])
            CTKUnMapCompilerInfo info
        endfor
        unlet b:compiler_info
    endif

"    call Dret('s:delete_ci')
endfunction

function! s:sub_info(info) " {{{2
"    call Decho('let '.get(a:info, 'name', 'cur_info').'['.submatch(1).'] '.
                \ submatch(2).'= "'.submatch(4).'"')

    let val = submatch(2) == '+' ? s:get_entry_val('', submatch(1), '') : ''
    let val = val == '' ? '' : val.' '
    let a:info[submatch(1)] = val.submatch(4)
endfunction

function! s:expand_var(entry, default) " {{{2
    let key = submatch(1) == '' ? submatch(3) : submatch(1)
    let val = s:get_entry_val(a:default ? '' : a:entry, key, submatch(0))

"    call Decho('replace '.key.' to '.val)
    if submatch(2) ==? 'q-'
        let escape_val = escape(val, '\"')
        return a:default ? escape_val : '"'.escape_val.'"'
    endif

    return val
endfunction

function! s:expand_env() " {{{2
    let key = submatch(1) == '' ? submatch(3) : submatch(1)
    let val = substitute(g:ctk_envvarfmt, '\<var\>', key, 'g')

"    call Decho('replace '.key.' to '.val)
    if submatch(2) ==? 'q-'
        return '"'.val.'"'
    endif
    return val
endfunction

function! s:expand_fname(fname, mode) " {{{2
"    call Dfunc('s:expand_fname(fname = '.a:fname.', mode = '.a:mode.')')
    let fname = expand(a:fname)

    if a:mode == ':'
        let fname = exists('*fnameescape') ? fnameescape(fname)
                    \ : escape(fname, s:fname_escape_chars)
    endif

    if fname =~ s:pat_fname_escape
        if v:version > 702 || v:version == 702 && has('patch111')
            let fname = shellescape(fname)

        " I hope it can work... but maybe you should update your version :-)
        elseif has('win32')
            let fname = '"'.substitute(fname, '"', '""', 'g').'"'
        else
            let fname = "'".fname."'"
        endif
    endif

"    call Dret('s:expand_fname : '.fname)
    return fname
endfunction

function! s:is_run_direct() " {{{2
    return get(get(get(b:{s:ci}, 'status', {}),
                \ 'info', {}), 'cmd', '') =~ s:pat_run_direct
endfunction

function! s:begin_setinfo() " {{{2
    if exists(s:ci_name) && get(b:{s:ci}, 'ft', '') != &ft
        call s:delete_ci()
    endif
endfunction

function! s:need_update() " {{{2
    return !exists(s:ci_name) || get(b:{s:ci}, 'ft', '') != &ft
endfunction

function! s:end_setinfo() " {{{2
    if exists(s:ci_name)
        let b:{s:ci}.ft = &ft
    endif
endfunction

function! s:set_fname() " {{{2
    if g:ctk_autofname == '' | return | endif

    " if we really have a filetype and this type isn't our debug window's
    " and we don't have a filename, and we have a compiler info, set auto
    " name.
    if !exists('b:ctk_fname')
        return &ft != '' && &ft !=? 'decho' && expand('%') == ''
                \ && exists('b:compiler_info') ? s:set_filename() : 0
    endif

"     call Decho("we are in set_fname(), buffer ".bufnr(""))
"     call Decho("we".(exists("b:compiler_info") ? "" : " don't")." have b:compiler_info")

    " if the name is autocreated and now we don't have a filetype,
    " delete the name
    if !b:ctk_fname[2] && (&ft == '' || !exists('b:compiler_info'))
        silent! noau 0file | unlet b:ctk_fname

    " if we have a filename and it isn't same as ctk_fname, set it to
    " user-defined (that is, the name is chnanged by user)
    elseif b:ctk_fname[0] != expand('%:p') || b:ctk_fname[2]
        let b:ctk_fname = [expand('%:p'), &ft, 1]

    " otherwise, call set_filename to set autocreated name
    elseif &ft !=? 'decho' && b:ctk_fname[1] != &ft
        call s:set_filename()
    endif
endfunction

function! s:set_filename() " {{{2
"    call Dfunc('s:set_filename()')

    if exists('b:'.g:ctk_ext_var) | let ext = b:{g:ctk_ext_var}
    elseif &sua != '' | let ext = &sua[1:]
    elseif &ft != '' | let ext = &ft
    else 
        let b:ctk_fname = [expand('%:p'), &ft, 0] 
        return
    endif

"    call Decho('ext = "'.ext.'"')
    let tempdir = get(b:, 'ctk_tempdir', g:ctk_tempdir)
    if !isdirectory(tempdir) && exists('*mkdir')
        call mkdir(tempdir, 'p')
    endif
    if !isdirectory(tempdir)
        let tempdir = '.'
    endif
    let tempdir = fnamemodify(tempdir, ':p')
"    call Decho('tempdir = "'.tempdir.'"')

    if exists('b:ctk_tempdir')
        let b:ctk_tempdir = tempdir
    else
        let g:ctk_tempdir = tempdir
    endif

    let fname = get(b:, 'ctk_autofname', g:ctk_autofname)
    let idx = 1
    while filereadable(tempdir.'/'.eval(fname).'.'.ext)
        let idx += 1
    endwhile
"    call Decho('fname = "'.eval(fname).'.'.ext.'"')

    if getcwd() == $VIMRUNTIME
"        call Decho('now we are in $VIMRUNTIME, just out of it...')
        exec 'lcd '.tempdir
"        call Decho('now we are in "'.getcwd().'"')
    endif

    silent exec 'file '.simplify(fnamemodify(tempdir.glob('/').
                \ eval(fname).'.'.ext, ':.'))

    let b:ctk_fname = [expand('%:p'), &ft, 0]
"    call Dret('s:set_filename')
endfunction

function! s:save_source() " {{{2
"    call Dfunc('s:save_source()')

    try 
        silent write 
"        call Dret('s:save_source : success')
        return 1

    catch /E13/ " File exists
        let res = s:question("File Exists, Overwrite?([Y]yes/[N]o/[C]ancel):")

    catch /E45/ " Readonly
        let res = s:question("File Readonly, Still write?([Y]yes/[N]o/[C]ancel):")

    catch
        call s:echoerr('error occur when save source: '.v:exception)
        let res = s:question("Force to write?([Y]yes/[N]o/[C]ancel):")

    endtry

    if res ==? 'y'
        try 
            silent write! 
"            call Dret('s:save_source : success')
            return 1
        catch | call s:echoerr("can't force save source: ".v:exception)
        endtry
    elseif res ==? 'n'
"        call Dret('s:save_source : success')
        return 1
    endif

"    call Dret('s:save_source : fail')
endfunction

function! s:make_info(info) " {{{2
    if expand('%') == '' | return | endif
"    call Dfunc('s:make_info(info = '.a:info.name.')')
    let cur_info = copy(a:info)
    for var in ['cmd', 'run', 'un']
        silent! unlet! cur_info[var.'map']
    endfor

    if exists('b:ctk_fname') && !b:ctk_fname[2]
"        call Decho('filename is an autogenerated name, use defoutput')
        let cur_info.output = g:ctk_defoutput
"    else | call Decho('filename is user-defined')
    endif

    " analyze the modeline to modifie the info
    if &modeline && &mls != 0
        let last = line('$')
        let old_info = get(b:{s:ci}.status, 'info', {})
        let b:{s:ci}.status.info = {}
        if last <= &mls * 2
            call s:read_modeline(cur_info, 1, last)
        else
            call s:read_modeline(cur_info, 0, &mls)
            call s:read_modeline(cur_info, last - &mls, last)
        endif
        let b:{s:ci}.status.info = old_info
    endif

"     call Dret('s:make_info : '.string(cur_info))
     return cur_info
endfunction

function! s:make_cmd(cmd, entry, use_native) " {{{2
"    call Dfunc('s:make_cmd(cmd = "'.a:cmd.'", entry = "'.a:entry.'")')

    let cmd = s:process_placeholder(a:cmd, a:entry)

    " this is just for "run"
    " defaultly, we use system toolkit to compile a program. that means, we
    " will execute a program directly, let OS to choose the right one to exec.
    " that usually the one in system folder (some places like /usr/bin).
    "
    " but, we should run currently generated program in "run", so we must
    " include the current folder for finding the program.
    if a:use_native && cmd[0] !~ ':'
"        call Decho('this is a executable cmd')
        let exe = matchstr(cmd, s:pat_shellcmdtitle)
"        call Decho('cmd = '.cmd.', exe = '.exe)

        " on unix, the program must has "./" prefix if it's at current folder
        if has('unix') && exe[:1] != './'
                    \ && glob(exe) != '' && executable('./'.exe)
            let cmd = './'.(cmd[0] == '!' ? cmd[1:] : cmd)
        endif

        " but ./foobar can't exec on windows :-(
        " so, take of the "./" prefix
        if has('win32') && cmd =~ '^!\=\./'
            let cmd = '!'.(cmd[0] == '!' ? cmd[3:] : cmd[2:])
        endif

        let cmd = cmd[0] != '!' ? '!'.cmd : cmd
    endif

"    call Dret('s:make_cmd : '.cmd)
    return cmd
endfunction

function! s:exec_cmd(cmdarg) " {{{2
"    call Dfunc('s:exec_cmd(cmdarg = '.a:cmdarg.')')

    let cmd = a:cmdarg
    let cmd_is_shell = (cmd =~ s:pat_cmd_is_shell)

    if g:ctk_cmdenc != '' && &enc != g:ctk_cmdenc && exists('*iconv')
                \ && cmd_is_shell
        let cmd = iconv(cmd, &enc, g:ctk_cmdenc)
    endif

    if cmd[0] == ':'
        if has('gui_running') || !cmd_is_shell
            redir => g:ctk_redir | silent! exec cmd | redir END
        else
            redir => g:ctk_redir | exec cmd | redir END
            if &term != 'linux' | redraw! | endif
        endif
        let output = g:ctk_redir
    else
        silent! let output = system(cmd[0] == '!' ? cmd[1:] : cmd)
    endif

    let output = matchstr(output, s:pat_execoutput)
"    call Dret('s:exec_cmd : '.output)
    return output
endfunction

function! s:show_list(info) " {{{2
    if empty(a:info) | return | endif

    echohl Title
    echo has_key(a:info, 'name') ?
                \ (has_key(a:info, 'title') ? 
                \ a:info.title."\n\tname         = ".a:info.name."\n"
                \ : a:info.name."\n")
                \ : "Default Values: ".&ft." files"
    echohl NONE

    for key in sort(filter(keys(a:info),
                \ "v:val !~ '".'^\%(title\|name\|unmap\)$'."'"))
        echo printf("\t%-12s = %s", key, a:info[key])
    endfor
endfunction

function! s:read_modeline(info, begin, end) " {{{2
"    call Dfunc('s:read_modeline(begin = '.a:begin.', end = '.a:end.')')
    let pos = winsaveview()

    call cursor(a:begin, 1)
    while search(s:pat_modeline, '', a:end) != 0
"        call Decho('find a modeline in line '.line('.'))
        let mlist = matchlist(getline('.'), s:pat_modeline)
        if mlist[1] == '' || a:info.name =~ '^\V'.escape(mlist[1], '\')
            call substitute(mlist[2], s:pat_info_var,
                        \ '\=s:sub_info(a:info)', 'g')
        endif
    endwhile

    call winrestview(pos)
"    call Dret('s:read_modeline')
endfunction

function! s:process_placeholder(cmd, entry) " {{{2
"    call Dfunc('s:process_placeholder(cmd = '.a:cmd.', entry = '.a:entry.')')
    let cmd = a:cmd

    let cmd = substitute(cmd, s:pat_cmdtag, '\=s:expand_var(a:entry, 0)', 'g')
    if a:entry != ''
        let cmd = substitute(cmd, s:pat_cmdtag, '\=s:expand_var(a:entry, 1)', 'g')
    endif
    let cmd = substitute(cmd, s:pat_filespec_nonescape,
                \ '\=s:expand_fname(submatch(0), cmd[0])', 'g')

    let cmd = substitute(cmd, s:pat_filespec_escape, '', 'g')
    if g:ctk_envvarfmt != ''
        let cmd = substitute(cmd, s:pat_cmdtag, '\=s:expand_env()', 'g')
    endif

"    call Dret('s:process_placeholder : '.cmd)
    return cmd
endfunction " }}}2

" ============================================================
function! s:add_flags(flags, count) " {{{1
    if a:count > 0 && a:count <= len(b:{s:ci}.list)
        let compiler = '-'.b:ctk.info[a:count - 1].name
    else
        let compiler = ''
    endif

    let com_begin = matchstr(&com, s:pat_com_begin)
    if com_begin != ''
        let com_begin .= ' '
        let com_end = ' '.matchstr(&com, s:pat_com_end)
    else
        let com_begin = matchstr(&com, s:pat_com).' '
        let com_end = ''
    endif
    
    call append(line('$'), com_begin.'cc'.compiler.': '.a:flags.com_end)
endfunction

function! s:list_compiler(name, idx) " {{{1
    " offer index, just show the speciafied info
    if a:0 != 0 && a:1 != 0
        return s:show_list(b:{s:ci}.list[a:1])
    endif

    let current = get(get(b:{s:ci}, 'status', {}), 'info', {})
    if (a:name == '' || a:name ==? 'current') && !empty(current)
        call s:show_list(current)
        return
    endif

    if a:name == '' || a:name ==? 'default' || a:name ==? 'all'
        call s:show_list(get(b:{s:ci}, 'default', {}))
        if a:name ==? 'default' | return | endif
    endif

    if a:name == '' || a:name ==? 'all'
        for info in b:{s:ci}.list
            call s:show_list(info)
        endfor
        return
    endif

    return s:show_list(s:get_info(a:name))
endfunction

function! s:set_default_info(cmdarg, bang) " {{{1
    if !s:need_update() | return | endif
"    call Dfunc('s:set_default_info(cmdarg = "'.a:cmdarg.'")')
    if !exists(s:ci_name)
        let b:{s:ci} = {}
    endif
    if a:bang == '!' || !has_key(b:{s:ci}, 'default')
        let b:{s:ci}.default = {}
    endif
    let def_info = b:{s:ci}.default

    call substitute(a:cmdarg, s:pat_info_var, '\=s:sub_info(def_info)', 'g')

    if has_key(def_info, 'extname')
        let b:{g:ctk_ext_var} = def_info['extname']
        unlet def_info['extname']
    endif

"    call Dret('s:set_default_info')
endfunction

function! s:set_compiler_info(cmdarg, bang) " {{{1
    if !s:need_update() | return | endif
"    call Dfunc('s:set_compiler_info(cmdarg = "'.a:cmdarg.'")')
    if !exists(s:ci_name)
        let b:{s:ci} = {'list':[]}
    elseif !has_key(b:{s:ci}, 'list')
        let b:{s:ci}.list = []
    endif

    " empty command
    if a:cmdarg == ''
        if a:bang == '!' 
            call s:delete_ci() 
        else
            call s:list_compiler('all', 0)
        endif
"        return Dret('s:set_compiler_info')
    endif

    " find name and others, mlist = [all, name, infos]
    let mlist = matchlist(a:cmdarg, s:pat_info)

    " add or modify a info
    if mlist[2] != ''
        let info = s:get_info(mlist[1])

        if a:bang == '!'
            let info = {}
        endif

        " add a new info, or clean old info
        if empty(info)
"            call Decho('add a new info')
            let info.name = mlist[1]
            call add(b:{s:ci}.list, info)
        else
"            call Decho('clear old info: '.string(info))
            CTKUnMapCompilerInfo info
            call filter(info, 0)
        endif

        let info.name = mlist[1]
        call substitute(mlist[2], s:pat_info_var, '\=s:sub_info(info)', 'g')

        let info.unmap = []
        let idx = s:get_idx(info)
        let dict = {'cmdmap': 'CC', 'runmap': 'RUN'}
        for key in keys(dict)
            if !has_key(info, key) | continue | endif
            let {key} = ''
"            call Decho('setup '.key)
            for mkey in s:get_escaped_list(info[key])
                let cpos = stridx(mkey, ':')
                for mode in split(cpos <= 0 ? 'nvi' : mkey[:cpos - 1], '\zs')
                    try | exec mode.'noremap <silent><unique> '.mkey[cpos+1:].
                                \ ' <C-\><C-N>:'.(idx+1).dict[key].'!<CR><C-\><C-G>'
                        call add(info.unmap, mode.mkey[cpos+1:])
                        let {key} .= mode.'map:'.mkey[cpos+1:].' '
                    catch | endtry
                endfor
            endfor
            if {key} == '' | unlet info[key]
            else | let info[key] = {key} | endif
        endfor

    " remove a info
    elseif a:bang == '!'
        let info = s:get_info(mlist[1])

        if !empty(info)
            CTKUnMapCompilerInfo info
            call remove(b:{s:ci}.list, s:get_idx(info))
        endif

    " list a info
    else
        call s:list_compiler(mlist[1], 0)
    endif

"    call Decho('>> now info = '.string(info))
"    call Dret('s:set_compiler_info')
endfunction

function! s:compile(count, entry, bang) " {{{1
    let ci = b:{s:ci}

    " find source buffer, and save it. if failed (return nonzero), echo
    " message and return 1 (failed, no use in this version)
    " NOTE: we find source in s:find_and_call() function, so we needn't find source
    " again. 
    if !s:save_source() || a:count < 0 || a:count > len(ci)
        redraw | echo 'Nothing Done'
        return 1
    endif

    if a:bang == '!' || !has_key(ci, 'status')
        let ci.status = {}
    endif
    let stat = ci.status

"    call Decho('current changenr = '.changenr().' and stat->changenr = '.get(stat, 'changenr', -1))
    if !s:is_run_direct()
                \ && get(stat, 'changenr', -1) == changenr()
                \ && (a:count == 0 || get(stat, 'idx', -1) == a:count - 1)
                \ && (a:entry == '' || get(stat, 'entry', '') == a:entry)
        redraw | echo 'Buffer no changed, Nothing Done.'
        return 0
    endif

"    call Dfunc('s:compile(count = '.a:count.', entry = '.a:entry.
                \ ', bang = '.a:bang.')')

    " init status variables
    let stat.changenr = changenr()
    let stat.entry = a:entry == '' ? get(stat, 'entry', '') : a:entry
    let stat.idx = a:count == 0 ? get(stat, 'idx', 0) : a:count - 1
    let stat.info = s:make_info(ci.list[stat.idx])

    if s:is_run_direct()
        call s:run(a:count, a:entry, a:bang)
        return 1
    endif

    " "entry" is just something like trigger. you press :CC entry, then
    " "entry" specified commands will be executed.
    redraw | echo 'Compiling ...'
    let ret_val = 1
    let msg = 'Compiling... using '.
                \ s:get_entry_val(stat.entry, 'title', stat.info.name)

    " don't use locale program
    let cmd = s:make_cmd(s:get_entry_val(stat.entry, 'cmd', ''), stat.entry, 0)
    if cmd == '' 
        redraw | echo 'Empty command, Nothing Done.' 
"        call Dret('s:compile')
        return  
    endif

    let res = s:exec_cmd(cmd)

    redraw
    if cmd[0] != ':'
"        call Decho('A shell command')
        let ret_val = v:shell_error

        if has('win32') && &enc != g:ctk_cmdenc && exists('*iconv')
            let res = iconv(res, g:ctk_cmdenc, &enc)
        endif

        cgetexpr [msg, cmd, ''] + split(res, "\<NL>")
                    \ + [stat.info.name.' returned '.ret_val]
        exec v:shell_error == 0 ? 'cwindow' : 
                    \ (res == '' ? 'cclose' : 'copen')

        echo 'Compile' (v:shell_error ? 'Fail' : 'Successful')

    elseif res != ''
"        call Decho('A exec command')
        echo msg."\n".cmd."\n\n".res
    endif

"    call Dret('s:compile : '.ret_val)
    return ret_val
endfunction

function! s:run(count, entry, bang) " {{{1
"    call Dfunc('s:run(count = '.a:count.', entry = '.a:entry.
                \ ', bang = '.a:bang.')')

    let bufnr = bufnr('%')
    if !s:is_run_direct() && s:compile(a:count, a:entry, a:bang) | return 1 | endif
    exec bufwinnr(bufnr).'wincmd w'

    " use locale program
    let cmd = s:make_cmd(s:get_entry_val(a:entry, 'run', ''), a:entry, 1)

    if cmd == '' 
        redraw | echo 'Empty command, Nothing Done.' 
"        call Dret('s:run')
        return
    endif

    if cmd[0] != ':'
        let cmd = cmd[0] == '!' ? cmd[1:] : cmd
        if g:ctk_execprg != ''
            let cmd = substitute(g:ctk_execprg, s:pat_exectag,
                        \ escape(cmd, '\'), 'g')
        endif
        let cmd = (has('gui_running') ? ':silent !' : ':!').cmd
    endif

    let res = s:exec_cmd(cmd)

    if &term != 'linux'
        if res !~ '^\_s*$' && !has('gui_running')
            redraw
            for line in split(res, '\n\|\r\|\r\n')
                echomsg line
            endfor
        endif
    endif

"    call Dret('s:run')
endfunction

" ======================================================{{{1

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: ff=unix ft=vim fdm=marker sw=4 ts=8 et sta nu
