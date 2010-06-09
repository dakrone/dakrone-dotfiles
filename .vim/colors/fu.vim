" Vim color file
"
" Name:       fu.vim
" Version:    1.0
" Maintainer:	Aaron Mueller <mail@aaron-mueller.de>
" 
" This is a compositon from railscast, mustang and xoria256 with a lot of
" improvemts in the colors. Color numbers (0-255) see:
" http://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html
"
" History:
"   2010-06-09  Initial setup and creation of this file. Additional colors for
"   Ruby and the diff view are added.

if &t_Co != 256 && ! has("gui_running")
  echomsg ""
  echomsg "err: please use GUI or a 256-color terminal (so that t_Co=256 could be set)"
  echomsg ""
  finish
endif

set background=dark
hi clear

if exists("syntax_on")
  syntax reset
endif
let colors_name = "fu"


"" General colors
hi Normal         ctermfg=252 ctermbg=234
hi CursorColumn               ctermbg=238 
hi Cursor                     ctermbg=214 
hi CursorLine                 ctermbg=238 
hi FoldColumn     ctermfg=248 ctermbg=bg  
hi Folded         ctermfg=255 ctermbg=60
hi IncSearch      ctermfg=0   ctermbg=223
hi NonText        ctermfg=248 ctermbg=233 cterm=bold
hi Search         ctermfg=0   ctermbg=149 
hi SignColumn     ctermfg=248
hi SpecialKey     ctermfg=77
hi StatusLine     ctermfg=232 ctermbg=255
hi StatusLineNC   ctermfg=237 ctermbg=253
hi TabLine        ctermfg=253  ctermbg=237
hi TabLineFill    ctermfg=0   ctermbg=0
hi TabLineSel     ctermfg=255 ctermbg=33
hi VertSplit      ctermfg=237 ctermbg=237
hi Visual         ctermfg=24  ctermbg=153 
hi VIsualNOS      ctermfg=24  ctermbg=153
hi LineNr         ctermfg=248 ctermbg=232
hi ModeMsg        ctermfg=220

hi ErrorMsg       ctermfg=196 ctermbg=52
hi SpellBad       ctermfg=196 ctermbg=52

if version >= 700
  hi CursorLine   ctermbg=236
  hi CursorColumn ctermbg=236
  hi MatchParen   ctermfg=157 ctermbg=237 cterm=bold
  hi Pmenu 		    ctermfg=255 ctermbg=236
  hi PmenuSel 	  ctermfg=0   ctermbg=74
  hi PmenuSbar                ctermbg=243
  hi PmenuThumb               ctermbg=252
  hi WildMenu     ctermfg=255 ctermbg=33
endif

" Syntax highlighting
hi Comment 		    ctermfg=244

hi Constant 	    ctermfg=220             cterm=bold
hi String         ctermfg=107 ctermbg=233
hi Character      ctermfg=228 ctermbg=16
hi Number		      ctermfg=214
hi Boolean        ctermfg=148

hi Identifier 	  ctermfg=149
hi Function 	    ctermfg=231

hi Statement 	    ctermfg=103
hi Conditional    ctermfg=105
hi Repeat         ctermfg=105
hi Label          ctermfg=105
hi Operator       ctermfg=243
hi Keyword		    ctermfg=190
hi Exception      ctermfg=166 ctermbg=0

hi PreProc 		    ctermfg=229

hi Type 		      ctermfg=111
hi Structure      ctermfg=111 ctermbg=233

hi Special		    ctermfg=220
hi SpecialComment ctermfg=228 ctermbg=16

hi Error          ctermfg=196 ctermbg=52
hi Todo           ctermfg=46  ctermbg=22

" Diff
hi diffAdded    ctermfg=150 
hi diffRemoved  ctermfg=174 
hi diffAdd      ctermfg=bg    ctermbg=151
hi diffDelete   ctermfg=bg    ctermbg=246
hi diffChange   ctermfg=bg    ctermbg=181
hi diffText     ctermfg=bg    ctermbg=174

" Ruby
hi rubyBlockParameter        ctermfg=27
hi rubyClass                 ctermfg=75
hi rubyConstant              ctermfg=167
hi rubyInstanceVariable      ctermfg=189
hi rubyInterpolation         ctermfg=107
hi rubyLocalVariableOrMethod ctermfg=189
hi rubyPredefinedConstant    ctermfg=167
hi rubyPseudoVariable        ctermfg=221
hi rubyStringDelimiter       ctermfg=143

