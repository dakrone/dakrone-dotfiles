" Vimwiki autoload plugin file
" Export to HTML
" Author: Maxim Kim <habamax@gmail.com>
" Home: http://code.google.com/p/vimwiki/

" Load only once {{{
if exists("g:loaded_vimwiki_html_auto") || &cp
  finish
endif
let g:loaded_vimwiki_html_auto = 1
"}}}

" Warn if html header or html footer do not exist only once. {{{
let s:warn_html_header = 0
let s:warn_html_footer = 0
"}}}
" TODO: move the next 2 functions into vimwiki#msg and
function! s:msg(message) "{{{
  echohl WarningMsg
  echomsg 'vimwiki: '.a:message
  echohl None
endfunction "}}}

function! s:root_path(subdir) "{{{
  return repeat('../', len(split(a:subdir, '[/\\]')))
endfunction "}}}

function! s:syntax_supported() " {{{
  return VimwikiGet('syntax') == "default"
endfunction " }}}

function! s:create_default_CSS(path) " {{{
  let path = expand(a:path)
  let css_full_name = path.VimwikiGet('css_name')
  if glob(css_full_name) == ""
    call vimwiki#mkdir(fnamemodify(css_full_name, ':p:h'))
    let lines = []

    call add(lines, 'body {font-family: Arial, sans-serif; margin: 1em 2em 1em 2em; font-size: 100%; line-height: 130%;}')
    call add(lines, 'h1, h2, h3, h4, h5, h6 {font-family: Trebuchet MS, serif; margin-top: 1.5em; margin-bottom: 0.5em;}')
    call add(lines, 'h1 {font-size: 2.0em; color: #3366aa;}')
    call add(lines, 'h2 {font-size: 1.6em; color: #335588;}')
    call add(lines, 'h3 {font-size: 1.2em; color: #224466;}')
    call add(lines, 'h4 {font-size: 1.1em; color: #113344;}')
    call add(lines, 'h5 {font-size: 1.0em; color: #112233;}')
    call add(lines, 'h6 {font-size: 1.0em; color: #111111;}')
    call add(lines, 'p, pre, blockquote, table, ul, ol, dl {margin-top: 1em; margin-bottom: 1em;}')
    call add(lines, 'ul ul, ul ol, ol ol, ol ul {margin-top: 0.5em; margin-bottom: 0.5em;}')
    call add(lines, 'li {margin: 0.3em auto;}')
    call add(lines, 'ul {margin-left: 2em; padding-left: 0.5em;}')
    call add(lines, 'dt {font-weight: bold;}')
    call add(lines, 'img {border: none;}')
    call add(lines, 'pre {border-left: 1px solid #ccc; margin-left: 2em; padding-left: 0.5em;}')
    call add(lines, 'blockquote {padding: 0.4em; background-color: #f6f5eb;}')
    call add(lines, 'td {border: 1px solid #ccc; padding: 0.3em;}')
    call add(lines, 'hr {border: none; border-top: 1px solid #ccc; width: 100%;}')
    call add(lines, '.todo {font-weight: bold; background-color: #f0ece8; color: #a03020;}')
    call add(lines, '.strike {text-decoration: line-through; color: #777777;}')
    call add(lines, '.justleft {text-align: left;}')
    call add(lines, '.justright {text-align: right;}')
    call add(lines, '.justcenter {text-align: center;}')

    call writefile(lines, css_full_name)
    echomsg "Default style.css is created."
  endif
endfunction "}}}

function! s:remove_blank_lines(lines) " {{{
  while a:lines[-1] =~ '^\s*$'
    call remove(a:lines, -1)
  endwhile
endfunction "}}}

function! s:is_web_link(lnk) "{{{
  if a:lnk =~ '^\(http://\|www.\|ftp://\)'
    return 1
  endif
  return 0
endfunction "}}}

function! s:is_img_link(lnk) "{{{
  if a:lnk =~ '\.\(png\|jpg\|gif\|jpeg\)$'
    return 1
  endif
  return 0
endfunction "}}}

function! s:is_non_wiki_link(lnk) "{{{
  " TODO: Add more file extensions here
  if a:lnk =~ '.\+\.\%(pdf\|txt\|doc\|rtf\|xls\)$'
    return 1
  endif
  return 0
endfunction "}}}

function! s:has_abs_path(fname) "{{{
  if a:fname =~ '\(^.:\)\|\(^/\)'
    return 1
  endif
  return 0
endfunction "}}}

function! s:get_html_header(wikifile, subdir, charset) "{{{
  let lines=[]

  let title = fnamemodify(a:wikifile, ":t:r")

  if VimwikiGet('html_header') != "" && !s:warn_html_header
    try
      let lines = readfile(expand(VimwikiGet('html_header')))
      call map(lines, 'substitute(v:val, "%title%", "'. title .'", "g")')
      call map(lines, 'substitute(v:val, "%root_path%", "'.
            \ s:root_path(a:subdir) .'", "g")')
      return lines
    catch /E484/
      let s:warn_html_header = 1
      call s:msg("Header template ".VimwikiGet('html_header').
            \ " does not exist!")
    endtry
  endif

  let css_name = expand(VimwikiGet('css_name'))
  let css_name = substitute(css_name, '\', '/', 'g')
  if !s:has_abs_path(css_name)
    " Relative css file for deep links: [[dir1/dir2/dir3/filename]]
    let css_name = s:root_path(a:subdir).css_name
  endif

  " if no VimwikiGet('html_header') set up or error while reading template
  " file -- use default header.
  call add(lines, '<html>')
  call add(lines, '<head>')
  call add(lines, '<link rel="Stylesheet" type="text/css" href="'.
        \ css_name.'" />')
  call add(lines, '<title>'.title.'</title>')
  call add(lines, '<meta http-equiv="Content-Type" content="text/html;'.
        \ ' charset='.a:charset.'" />')
  call add(lines, '</head>')
  call add(lines, '<body>')

  return lines
endfunction "}}}

function! s:get_html_footer() "{{{
  let lines=[]

  if VimwikiGet('html_footer') != "" && !s:warn_html_footer
    try
      let lines = readfile(expand(VimwikiGet('html_footer')))
      return lines
    catch /E484/
      let s:warn_html_footer = 1
      call s:msg("Footer template ".VimwikiGet('html_footer').
            \ " does not exist!")
    endtry
  endif

  " if no VimwikiGet('html_footer') set up or error while reading template
  " file -- use default footer.
  call add(lines, "")
  call add(lines, '</body>')
  call add(lines, '</html>')

  return lines
endfunction "}}}

function! s:close_tag_pre(pre, ldest) "{{{
  if a:pre
    call insert(a:ldest, "</pre></code>")
    return 0
  endif
  return a:pre
endfunction "}}}

function! s:close_tag_quote(quote, ldest) "{{{
  if a:quote
    call insert(a:ldest, "</blockquote>")
    return 0
  endif
  return a:quote
endfunction "}}}

function! s:close_tag_para(para, ldest) "{{{
  if a:para
    call insert(a:ldest, "</p>")
    return 0
  endif
  return a:para
endfunction "}}}

function! s:close_tag_table(table, ldest) "{{{
  if a:table
    call insert(a:ldest, "</table>")
    return 0
  endif
  return a:table
endfunction "}}}

function! s:close_tag_list(lists, ldest) "{{{
  while len(a:lists)
    let item = remove(a:lists, -1)
    call insert(a:ldest, item[0])
  endwhile
endfunction! "}}}

function! s:close_tag_def_list(deflist, ldest) "{{{
  if a:deflist
    call insert(a:ldest, "</dl>")
    return 0
  endif
  return a:deflist
endfunction! "}}}

function! s:process_tag_pre(line, pre) "{{{
  let lines = []
  let pre = a:pre
  let processed = 0
  if !pre && a:line =~ '{{{[^\(}}}\)]*\s*$'
    let class = matchstr(a:line, '{{{\zs.*$')
    let class = substitute(class, '\s\+$', '', 'g')
    if class != ""
      call add(lines, "<pre ".class.">")
    else
      call add(lines, "<pre>")
    endif
    let pre = 1
    let processed = 1
  elseif pre && a:line =~ '^}}}\s*$'
    let pre = 0
    call add(lines, "</pre>")
    let processed = 1
  elseif pre
    let processed = 1
    call add(lines, a:line)
  endif
  return [processed, lines, pre]
endfunction "}}}

function! s:process_tag_quote(line, quote) "{{{
  let lines = []
  let quote = a:quote
  let processed = 0
  if a:line =~ '^\s\{4,}[^[:blank:]*#]'
    if !quote
      call add(lines, "<blockquote>")
      let quote = 1
    endif
    let processed = 1
    call add(lines, substitute(a:line, '^\s*', '', ''))
  elseif quote && a:line =~ '^\s*$'
    let processed = 1
    call add(lines, a:line)
  elseif quote
    call add(lines, "</blockquote>")
    let quote = 0
  endif
  return [processed, lines, quote]
endfunction "}}}

function! s:process_tag_list(line, lists) "{{{

  function! s:add_strike(line, st_tag, en_tag) "{{{
    let st_tag = a:st_tag
    let en_tag = a:en_tag
    " apply strikethrough for checked list items
    if a:line =~ '^\s\+\%(\*\|#\)\s*\[x]'
      let st_tag = a:st_tag.'<span class="strike">'
      let en_tag = '</span>'.a:en_tag
    endif
    return [st_tag, en_tag]
  endfunction "}}}

  function! s:add_checkbox(line, rx_list, st_tag, en_tag) "{{{
    let st_tag = a:st_tag
    let en_tag = a:en_tag

    let chk = matchlist(a:line, a:rx_list)
    if len(chk) > 0
      if chk[1] == 'x'
        let st_tag .= '<input type="checkbox" checked />'
      else
        let st_tag .= '<input type="checkbox" />'
      endif
    endif
    return [st_tag, en_tag]
  endfunction "}}}

  let lines = []
  let processed = 0

  if a:line =~ '^\s\+\*'
    let lstSym = '*'
    let lstTagOpen = '<ul>'
    let lstTagClose = '</ul>'
    let lstRegExp = '^\s\+\*\s*'
  elseif a:line =~ '^\s\+#'
    let lstSym = '#'
    let lstTagOpen = '<ol>'
    let lstTagClose = '</ol>'
    let lstRegExp = '^\s\+#\s*'
  else
    let lstSym = ''
    let lstTagOpen = ''
    let lstTagClose = ''
    let lstRegExp = ''
  endif

  let in_list = (len(a:lists) > 0)
  if lstSym != ''
    " To get proper indent level 'retab' the line -- change all tabs
    " to spaces*tabstop
    let line = substitute(a:line, '\t', repeat(' ', &tabstop), 'g')
    let indent = stridx(line, lstSym)

    let checkbox = '\s*\[\(.\?\)]\s*'
    let [st_tag, en_tag] = s:add_strike(line, '<li>', '</li>')
    let [st_tag, en_tag] = s:add_checkbox(line,
          \ lstRegExp.checkbox, st_tag, en_tag)

    if !in_list
      call add(a:lists, [lstTagClose, indent])
      call add(lines, lstTagOpen)
    elseif (in_list && indent > a:lists[-1][1])
      let item = remove(a:lists, -1)
      call add(lines, item[0])

      call add(a:lists, [lstTagClose, indent])
      call add(lines, lstTagOpen)
    elseif (in_list && indent < a:lists[-1][1])
      while len(a:lists) && indent < a:lists[-1][1]
        let item = remove(a:lists, -1)
        call add(lines, item[0])
      endwhile
    elseif in_list
      let item = remove(a:lists, -1)
      call add(lines, item[0])
    endif

    call add(a:lists, [en_tag, indent])
    call add(lines, st_tag)
    call add(lines,
          \ substitute(a:line, lstRegExp.'\%('.checkbox.'\)\?', '', ''))
    let processed = 1
  elseif in_list > 0 && a:line =~ '^\s\+\S\+'
    if g:vimwiki_list_ignore_newline
      call add(lines, a:line)
    else
      call add(lines, '<br />'.a:line)
    endif
    let processed = 1
  else
    while len(a:lists)
      let item = remove(a:lists, -1)
      call add(lines, item[0])
    endwhile
  endif
  return [processed, lines]
endfunction "}}}

function! s:process_tag_def_list(line, deflist) "{{{
  let lines = []
  let deflist = a:deflist
  let processed = 0
  let matches = matchlist(a:line, '\(^.*\)::\%(\s\|$\)\(.*\)')
  if !deflist && len(matches) > 0
    call add(lines, "<dl>")
    let deflist = 1
  endif
  if deflist && len(matches) > 0
    if matches[1] != ''
      call add(lines, "<dt>".matches[1]."</dt>")
    endif
    if matches[2] != ''
      call add(lines, "<dd>".matches[2]."</dd>")
    endif
    let processed = 1
  elseif deflist
    let deflist = 0
    call add(lines, "</dl>")
  endif
  return [processed, lines, deflist]
endfunction "}}}

function! s:process_tag_para(line, para) "{{{
  let lines = []
  let para = a:para
  let processed = 0
  if a:line =~ '^\S'
    if !para
      call add(lines, "<p>")
      let para = 1
    endif
    let processed = 1
    call add(lines, a:line)
  elseif para && a:line =~ '^\s*$'
    call add(lines, "</p>")
    let para = 0
  endif
  return [processed, lines, para]
endfunction "}}}

function! s:process_tag_h(line) "{{{
  let line = a:line
  let processed = 0
  let h_level = 0
  if a:line =~ g:vimwiki_rxH6
    let h_level = 6
  elseif a:line =~ g:vimwiki_rxH5
    let h_level = 5
  elseif a:line =~ g:vimwiki_rxH4
    let h_level = 4
  elseif a:line =~ g:vimwiki_rxH3
    let h_level = 3
  elseif a:line =~ g:vimwiki_rxH2
    let h_level = 2
  elseif a:line =~ g:vimwiki_rxH1
    let h_level = 1
  endif
  if h_level > 0
    " rtrim
    let line = substitute(a:line, '\s\+$', '', 'g')
    let line = '<h'.h_level.'>'.
          \ strpart(line, h_level, len(line) - h_level * 2).
          \'</h'.h_level.'>'
    let processed = 1
  endif
  return [processed, line]
endfunction "}}}

function! s:process_tag_hr(line) "{{{
  let line = a:line
  let processed = 0
  if a:line =~ '^-----*$'
    let line = '<hr />'
    let processed = 1
  endif
  return [processed, line]
endfunction "}}}

function! s:process_tag_table(line, table) "{{{
  let table = a:table
  let lines = []
  let processed = 0
  if a:line =~ '^||.\+||.*'
    if !table
      call add(lines, "<table>")
      let table = 1
    endif
    let processed = 1

    call add(lines, "<tr>")
    let pos1 = 0
    let pos2 = 0
    let done = 0
    while !done
      let pos1 = stridx(a:line, '||', pos2)
      let pos2 = stridx(a:line, '||', pos1+2)
      if pos1==-1 || pos2==-1
        let done = 1
        let pos2 = len(a:line)
      endif
      let line = strpart(a:line, pos1+2, pos2-pos1-2)
      if line == ''
        continue
      endif
      if strpart(line, 0, 1) == ' ' &&
            \ strpart(line, len(line) - 1, 1) == ' '
        call add(lines, '<td class="justcenter">'.line.'</td>')
      elseif strpart(line, 0, 1) == ' '
        call add(lines, '<td class="justright">'.line.'</td>')
      else
        call add(lines, '<td class="justleft">'.line.'</td>')
      endif
    endwhile
    call add(lines, "</tr>")

  elseif table
    call add(lines, "</table>")
    let table = 0
  endif
  return [processed, lines, table]
endfunction "}}}

function! s:process_tags(line) "{{{
  let line = a:line
  let line = s:make_tag(line, '\[\[.\{-}\]\]',
        \ '', '', 2, 's:make_internal_link')
  let line = s:make_tag(line, '\[.\{-}\]', '', '', 1, 's:make_external_link')
  let line = s:make_tag(line, g:vimwiki_rxWeblink,
        \ '', '', 0, 's:make_barebone_link')
  let line = s:make_tag(line, '!\?'.g:vimwiki_rxWikiWord,
        \ '', '', 0, 's:make_wikiword_link')
  let line = s:make_tag(line, g:vimwiki_rxItalic, '<em>', '</em>')
  let line = s:make_tag(line, g:vimwiki_rxBold, '<strong>', '</strong>')
  let line = s:make_tag(line, g:vimwiki_rxTodo,
        \ '<span class="todo">', '</span>', 0)
  let line = s:make_tag(line, g:vimwiki_rxDelText,
        \ '<span class="strike">', '</span>', 2)
  let line = s:make_tag(line, g:vimwiki_rxSuperScript,
        \ '<sup><small>', '</small></sup>', 1)
  let line = s:make_tag(line, g:vimwiki_rxSubScript,
        \ '<sub><small>', '</small></sub>', 2)
  let line = s:make_tag(line, g:vimwiki_rxCode, '<code>', '</code>')
  " TODO: change make_tag function: delete cSym parameter -- count of symbols
  " to strip from 2 sides of tag. Add 2 new instead -- OpenWikiTag length
  " and CloseWikiTag length as for preformatted text there could be {{{,}}}
  " and <pre>,</pre>.
  let line = s:make_tag(line, g:vimwiki_rxPreStart.'.\+'.g:vimwiki_rxPreEnd,
        \ '<code>', '</code>', 3)
  return line
endfunction " }}}

function! s:safe_html(line) "{{{
  "" change dangerous html symbols: < > &

  let line = substitute(a:line, '&', '\&amp;', 'g')
  let line = substitute(line, '<', '\&lt;', 'g')
  let line = substitute(line, '>', '\&gt;', 'g')
  return line
endfunction "}}}

function! s:make_tag_helper(line, regexp_match,
      \ tagOpen, tagClose, cSymRemove, func) " {{{
  "" Substitute text found by regexp_match with tagOpen.regexp_subst.tagClose

  let pos = 0
  let lines = split(a:line, a:regexp_match, 1)
  let res_line = ""
  for line in lines
    let res_line = res_line.line
    let matched = matchstr(a:line, a:regexp_match, pos)
    if matched != ""
      let toReplace = strpart(matched,
            \ a:cSymRemove, len(matched) - 2 * a:cSymRemove)
      if a:func!=""
        let toReplace = {a:func}(toReplace)
      else
        let toReplace = a:tagOpen.toReplace.a:tagClose
      endif
      let res_line = res_line.toReplace
    endif
    let pos = matchend(a:line, a:regexp_match, pos)
  endfor
  return res_line

endfunction " }}}

function! s:make_tag(line, regexp_match, tagOpen, tagClose, ...) " {{{
  "" Make tags only if not in ` ... `
  "" ... should be function that process regexp_match deeper.

  "check if additional function exists
  let func = ""
  let cSym = 1
  if a:0 == 2
    let cSym = a:1
    let func = a:2
  elseif a:0 == 1
    let cSym = a:1
  endif

  let patt_splitter = '\(`[^`]\+`\)\|\({{{.\+}}}\)\|'.
        \ '\(<a href.\{-}</a>\)\|\(<img src.\{-}/>\)'
  if '`[^`]\+`' == a:regexp_match || '{{{.\+}}}' == a:regexp_match
    let res_line = s:make_tag_helper(a:line, a:regexp_match,
          \ a:tagOpen, a:tagClose, cSym, func)
  else
    let pos = 0
    " split line with patt_splitter to have parts of line before and after
    " href links, preformatted text
    " ie:
    " hello world `is just a` simple <a href="link.html">type of</a> prg.
    " result:
    " ['hello world ', ' simple ', 'type of', ' prg']
    let lines = split(a:line, patt_splitter, 1)
    let res_line = ""
    for line in lines
      let res_line = res_line.s:make_tag_helper(line, a:regexp_match,
            \ a:tagOpen, a:tagClose, cSym, func)
      let res_line = res_line.matchstr(a:line, patt_splitter, pos)
      let pos = matchend(a:line, patt_splitter, pos)
    endfor
  endif
  return res_line
endfunction " }}}

function! s:make_external_link(entag) "{{{
  "" Make <a href="link">link desc</a>
  "" from [link link desc]

  let line = ''
  if s:is_web_link(a:entag)
    let lnkElements = split(a:entag)
    let head = lnkElements[0]
    let rest = join(lnkElements[1:])
    if rest==""
      let rest=head
    endif
    if s:is_img_link(rest)
      if rest!=head
        let line = '<a href="'.head.'"><img src="'.rest.'" /></a>'
      else
        let line = '<img src="'.rest.'" />'
      endif
    else
      let line = '<a href="'.head.'">'.rest.'</a>'
    endif
  elseif s:is_img_link(a:entag)
    let line = '<img src="'.a:entag.'" />'
  else
    " [alskfj sfsf] shouldn't be a link. So return it as it was --
    " enclosed in [...]
    let line = '['.a:entag.']'
  endif
  return line
endfunction "}}}

function! s:make_internal_link(entag) "{{{
  " Make <a href="This is a link">This is a link</a>
  " from [[This is a link]]
  " Make <a href="link">This is a link</a>
  " from [[link|This is a link]]
  " Make <a href="link">This is a link</a>
  " from [[link][This is a link]]
  " TODO: rename function -- it makes not only internal links.
  " TODO: refactor it.

  let line = ''
  if a:entag =~ '|'
    let link_parts = split(a:entag, "|", 1)
  else
    let link_parts = split(a:entag, "][", 1)
  endif

  if len(link_parts) > 1
    if len(link_parts) < 3
      let style = ""
    else
      let style = link_parts[2]
    endif

    if s:is_img_link(link_parts[1])
      let line = '<a href="'.link_parts[0].'"><img src="'.link_parts[1].
            \ '" style="'.style.'" /></a>'
    elseif len(link_parts) < 3
      if s:is_non_wiki_link(link_parts[0])
        let line = '<a href="'.link_parts[0].'">'.link_parts[1].'</a>'
      else
        let line = '<a href="'.vimwiki#safe_link(link_parts[0]).
              \ '.html">'.link_parts[1].'</a>'
      endif
    elseif s:is_img_link(link_parts[0])
      let line = '<img src="'.link_parts[0].'" alt="'.
            \ link_parts[1].'" style="'.style.'" />'
    endif
  else
    if s:is_img_link(a:entag)
      let line = '<img src="'.a:entag.'" />'
    elseif s:is_non_wiki_link(link_parts[0])
      let line = '<a href="'.a:entag.'">'.a:entag.'</a>'
    else
      let line = '<a href="'.vimwiki#safe_link(a:entag).
            \ '.html">'.a:entag.'</a>'
    endif
  endif
  return line
endfunction "}}}

function! s:make_wikiword_link(entag) "{{{
  " Make <a href="WikiWord">WikiWord</a> from WikiWord
  " if first symbol is ! then remove it and make no link.
  if a:entag[0] == '!'
    return a:entag[1:]
  else
    let line = '<a href="'.a:entag.'.html">'.a:entag.'</a>'
    return line
  endif
endfunction "}}}

function! s:make_barebone_link(entag) "{{{
  "" Make <a href="http://habamax.ru">http://habamax.ru</a>
  "" from http://habamax.ru

  if s:is_img_link(a:entag)
    let line = '<img src="'.a:entag.'" />'
  else
    let line = '<a href="'.a:entag.'">'.a:entag.'</a>'
  endif
  return line
endfunction "}}}

function! s:get_html_from_wiki_line(line, para, quote, pre,
      \ table, lists, deflist) " {{{
  let para = a:para
  let quote = a:quote
  let pre = a:pre
  let table = a:table
  let lists = a:lists
  let deflist = a:deflist

  let res_lines = []

  let line = s:safe_html(a:line)

  let processed = 0
  "" pre
  if !processed
    let [processed, lines, pre] = s:process_tag_pre(line, pre)
    if processed && len(lists)
      call s:close_tag_list(lists, lines)
    endif
    if processed && table
      let table = s:close_tag_table(table, lines)
    endif
    if processed && deflist
      let deflist = s:close_tag_def_list(deflist, lines)
    endif
    if processed && quote
      let quote = s:close_tag_quote(quote, lines)
    endif
    if processed && para
      let para = s:close_tag_para(para, lines)
    endif
    call extend(res_lines, lines)
  endif

  "" list
  if !processed
    let [processed, lines] = s:process_tag_list(line, lists)
    if processed && quote
      let quote = s:close_tag_quote(quote, lines)
    endif
    if processed && pre
      let pre = s:close_tag_pre(pre, lines)
    endif
    if processed && table
      let table = s:close_tag_table(table, lines)
    endif
    if processed && deflist
      let deflist = s:close_tag_def_list(deflist, lines)
    endif
    if processed && para
      let para = s:close_tag_para(para, lines)
    endif

    call map(lines, 's:process_tags(v:val)')

    call extend(res_lines, lines)
  endif

  "" quote
  if !processed
    let [processed, lines, quote] = s:process_tag_quote(line, quote)
    if processed && len(lists)
      call s:close_tag_list(lists, lines)
    endif
    if processed && deflist
      let deflist = s:close_tag_def_list(deflist, lines)
    endif
    if processed && table
      let table = s:close_tag_table(table, lines)
    endif
    if processed && pre
      let pre = s:close_tag_pre(pre, lines)
    endif
    if processed && para
      let para = s:close_tag_para(para, lines)
    endif

    call extend(res_lines, lines)
  endif

  "" definition lists
  if !processed
    let [processed, lines, deflist] = s:process_tag_def_list(line, deflist)

    call map(lines, 's:process_tags(v:val)')

    call extend(res_lines, lines)
  endif

  "" table
  if !processed
    let [processed, lines, table] = s:process_tag_table(line, table)

    call map(lines, 's:process_tags(v:val)')

    call extend(res_lines, lines)
  endif

  if !processed
    let [processed, line] = s:process_tag_h(line)
    if processed
      call s:close_tag_list(lists, res_lines)
      let table = s:close_tag_table(table, res_lines)
      let pre = s:close_tag_pre(pre, res_lines)
      call add(res_lines, line)
    endif
  endif

  if !processed
    let [processed, line] = s:process_tag_hr(line)
    if processed
      call s:close_tag_list(lists, res_lines)
      let table = s:close_tag_table(table, res_lines)
      let pre = s:close_tag_pre(pre, res_lines)
      call add(res_lines, line)
    endif
  endif

  "" P
  if !processed
    let [processed, lines, para] = s:process_tag_para(line, para)
    if processed && len(lists)
      call s:close_tag_list(lists, lines)
    endif
    if processed && quote
      let quote = s:close_tag_quote(quote, res_lines)
    endif
    if processed && pre
      let pre = s:close_tag_pre(pre, res_lines)
    endif
    if processed && table
      let table = s:close_tag_table(table, res_lines)
    endif

    call map(lines, 's:process_tags(v:val)')

    call extend(res_lines, lines)
  endif

  "" add the rest
  if !processed
    call add(res_lines, line)
  endif

  return [res_lines, para, quote, pre, table, lists, deflist]

endfunction " }}}

function! s:remove_comments(lines) "{{{
  let res = []
  let multiline_comment = 0

  let idx = 0
  while idx < len(a:lines)
    let line = a:lines[idx]
    let idx += 1

    if multiline_comment
      let col = matchend(line, '-->',)
      if col != -1
        let multiline_comment = 0
        let line = strpart(line, col)
      else
        continue
      endif
    endif

    if !multiline_comment && line =~ '<!--.*-->'
      let line = substitute(line, '<!--.*-->', '', 'g')
      if line =~ '^\s*$'
        continue
      endif
    endif

    if !multiline_comment
      let col = match(line, '<!--',)
      if col != -1
        let multiline_comment = 1
        let line = strpart(line, 1, col - 1)
      endif
    endif

    call add(res, line)
  endwhile
  return res
endfunction "}}}

function! vimwiki_html#Wiki2HTML(path, wikifile) "{{{

  if !s:syntax_supported()
    call s:msg('Only vimwiki_default syntax supported!!!')
    return
  endif

  let wikifile = fnamemodify(a:wikifile, ":p")
  let subdir = vimwiki#subdir(VimwikiGet('path'), wikifile)

  let path = expand(a:path).subdir
  call vimwiki#mkdir(path)

  let lsource = s:remove_comments(readfile(wikifile))
  let ldest = s:get_html_header(wikifile, subdir, &fileencoding)


  let para = 0
  let quote = 0
  let pre = 0
  let table = 0
  let deflist = 0
  let lists = []

  for line in lsource
    let oldquote = quote
    let [lines, para, quote, pre, table, lists, deflist] =
          \ s:get_html_from_wiki_line(line, para, quote, pre,
          \ table, lists, deflist)

    " A dirty hack: There could be a lot of empty strings before
    " s:process_tag_quote find out `pre` is over. So we should delete
    " them all. Think of the way to refactor it out.
    if (oldquote != quote) && ldest[-1] =~ '^\s*$'
      call s:remove_blank_lines(ldest)
    endif

    call extend(ldest, lines)
  endfor

  call s:remove_blank_lines(ldest)

  "" process end of file
  "" close opened tags if any
  let lines = []
  call s:close_tag_quote(quote, lines)
  call s:close_tag_para(para, lines)
  call s:close_tag_pre(pre, lines)
  call s:close_tag_list(lists, lines)
  call s:close_tag_def_list(deflist, lines)
  call s:close_tag_table(table, lines)
  call extend(ldest, lines)

  call extend(ldest, s:get_html_footer())

  "" make html file.
  let wwFileNameOnly = fnamemodify(wikifile, ":t:r")
  call writefile(ldest, path.wwFileNameOnly.'.html')
endfunction "}}}

function! vimwiki_html#WikiAll2HTML(path) "{{{
  if !s:syntax_supported()
    call s:msg('Only vimwiki_default syntax supported!!!')
    return
  endif

  let path = expand(a:path)
  call vimwiki#mkdir(path)

  let setting_more = &more
  setlocal nomore

  let wikifiles = split(glob(VimwikiGet('path').'**/*'.VimwikiGet('ext')), '\n')
  for wikifile in wikifiles
    echomsg 'Processing '.wikifile
    call vimwiki_html#Wiki2HTML(path, wikifile)
  endfor
  call s:create_default_CSS(path)
  echomsg 'Done!'

  let &more = setting_more
endfunction "}}}
