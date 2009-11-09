" Vimwiki syntax file
" Author: Maxim Kim <habamax@gmail.com>
" Home: http://code.google.com/p/vimwiki/
" vim:tw=79:

" Quit if syntax file is already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

"" use max highlighting - could be quite slow if there are too many wikifiles
if VimwikiGet('maxhi')
  " Every WikiWord is nonexistent
  if g:vimwiki_camel_case
    execute 'syntax match wikiNoExistsWord /\%(^\|[^!]\)\zs'.g:vimwiki_word1.'/'
  endif
  execute 'syntax match wikiNoExistsWord /'.g:vimwiki_word2.'/'
  execute 'syntax match wikiNoExistsWord /'.g:vimwiki_word3.'/'
  " till we find them in vimwiki's path
  call vimwiki#WikiHighlightWords()
else
  " A WikiWord (unqualifiedWikiName)
  execute 'syntax match wikiWord /\%(^\|[^!]\)\zs\<'.g:vimwiki_word1.'\>/'
  " A [[bracketed wiki word]]
  execute 'syntax match wikiWord /'.g:vimwiki_word2.'/'
endif

execute 'syntax match wikiLink `'.g:vimwiki_rxWeblink.'`'

" Emoticons: must come after the Textilisms, as later rules take precedence
" over earlier ones. This match is an approximation for the ~70 distinct
syntax match wikiEmoticons /\%((.)\|:[()|$@]\|:-[DOPS()\]|$@]\|;)\|:'(\)/

let g:vimwiki_rxTodo = '\C\%(TODO:\|DONE:\|FIXME:\|FIXED:\|XXX:\)'
execute 'syntax match wikiTodo /'. g:vimwiki_rxTodo .'/'

" Load concrete Wiki syntax
execute 'runtime! syntax/vimwiki_'.VimwikiGet('syntax').'.vim'

" Tables
execute 'syntax match wikiTable /'.g:vimwiki_rxTable.'/'

execute 'syntax match wikiBold /'.g:vimwiki_rxBold.'/'

execute 'syntax match wikiItalic /'.g:vimwiki_rxItalic.'/'

execute 'syntax match wikiBoldItalic /'.g:vimwiki_rxBoldItalic.'/'

execute 'syntax match wikiItalicBold /'.g:vimwiki_rxItalicBold.'/'

execute 'syntax match wikiDelText /'.g:vimwiki_rxDelText.'/'

execute 'syntax match wikiSuperScript /'.g:vimwiki_rxSuperScript.'/'

execute 'syntax match wikiSubScript /'.g:vimwiki_rxSubScript.'/'

execute 'syntax match wikiCode /'.g:vimwiki_rxCode.'/'

" <hr> horizontal rule
execute 'syntax match wikiHR /'.g:vimwiki_rxHR.'/'

" List items
execute 'syntax match wikiList /'.g:vimwiki_rxListBullet.'/'
execute 'syntax match wikiList /'.g:vimwiki_rxListNumber.'/'
execute 'syntax match wikiList /'.g:vimwiki_rxListDefine.'/'

execute 'syntax region wikiPre start=/'.g:vimwiki_rxPreStart.
      \ '/ end=/'.g:vimwiki_rxPreEnd.'/ contains=wikiComment'

" List item checkbox
syntax match wikiCheckBox /\[.\?\]/
if g:vimwiki_hl_cb_checked
  execute 'syntax match wikiCheckBoxDone /'.g:vimwiki_rxListBullet.'\s*\[x\].*$/'
  execute 'syntax match wikiCheckBoxDone /'.g:vimwiki_rxListNumber.'\s*\[x\].*$/'
endif

syntax region wikiComment start='<!--' end='-->'

if !vimwiki#hl_exists("wikiHeader1")
  execute 'syntax match wikiHeader /'.g:vimwiki_rxHeader.'/'
else
  " Header levels, 1-6
  execute 'syntax match wikiHeader1 /'.g:vimwiki_rxH1.'/'
  execute 'syntax match wikiHeader2 /'.g:vimwiki_rxH2.'/'
  execute 'syntax match wikiHeader3 /'.g:vimwiki_rxH3.'/'
  execute 'syntax match wikiHeader4 /'.g:vimwiki_rxH4.'/'
  execute 'syntax match wikiHeader5 /'.g:vimwiki_rxH5.'/'
  execute 'syntax match wikiHeader6 /'.g:vimwiki_rxH6.'/'
endif

if !vimwiki#hl_exists("wikiHeader1")
  hi def link wikiHeader Title
else
  hi def link wikiHeader1 Title
  hi def link wikiHeader2 Title
  hi def link wikiHeader3 Title
  hi def link wikiHeader4 Title
  hi def link wikiHeader5 Title
  hi def link wikiHeader6 Title
endif

hi def wikiBold term=bold cterm=bold gui=bold
hi def wikiItalic term=italic cterm=italic gui=italic
hi def wikiBoldItalic term=bold cterm=bold gui=bold,italic
hi def link wikiItalicBold wikiBoldItalic

hi def link wikiCode PreProc
hi def link wikiWord Underlined
hi def link wikiNoExistsWord Error

hi def link wikiPre PreProc
hi def link wikiLink Underlined
hi def link wikiList Statement
" hi def link wikiList Identifier
hi def link wikiCheckBox wikiList
hi def link wikiCheckBoxDone Comment
hi def link wikiTable PreProc
hi def link wikiEmoticons Character
hi def link wikiDelText Constant
hi def link wikiSuperScript Number
hi def link wikiSubScript Number
hi def link wikiTodo Todo
hi def link wikiComment Comment

let b:current_syntax="vimwiki"
