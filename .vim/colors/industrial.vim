" Vim color file

set background=dark
hi clear
if exists("syntax_on")
   syntax reset
endif

let colors_name = "industrial"

hi Normal          ctermfg=NONE ctermbg=NONE gui=NONE guifg=#ffffff guibg=#222222

" Search
hi IncSearch       ctermfg=NONE ctermbg=NONE gui=NONE guifg=#222222 guibg=#3399ee
hi Search          ctermfg=NONE ctermbg=NONE gui=NONE guifg=#222222 guibg=#99ee33

" Messages
hi ErrorMsg        ctermfg=NONE ctermbg=NONE gui=NONE guifg=#ff5544 guibg=NONE
hi WarningMsg      ctermfg=NONE ctermbg=NONE gui=NONE guifg=#ffaa66 guibg=NONE
hi ModeMsg         ctermfg=NONE ctermbg=NONE gui=NONE guifg=#ffffff guibg=NONE
hi MoreMsg         ctermfg=NONE ctermbg=NONE gui=NONE guifg=#ffffff guibg=NONE
hi Question        ctermfg=NONE ctermbg=NONE gui=NONE guifg=#ffffff guibg=NONE

" Split area
hi StatusLine      ctermfg=NONE ctermbg=NONE gui=NONE guifg=#ffffff guibg=#223344
hi StatusLineNC    ctermfg=NONE ctermbg=NONE gui=NONE guifg=#666666 guibg=#111111
hi VertSplit       ctermfg=NONE ctermbg=NONE gui=NONE guifg=#111111 guibg=#111111
hi WildMenu        ctermfg=NONE ctermbg=NONE gui=NONE guifg=#ffffff guibg=#336699

" Diff
hi DiffText        ctermfg=NONE ctermbg=NONE gui=NONE guifg=#07cfef guibg=#00151f
hi DiffChange      ctermfg=NONE ctermbg=NONE gui=NONE guifg=#ff97ff guibg=#2f002f
hi DiffDelete      ctermfg=NONE ctermbg=NONE gui=NONE guifg=#dfdf00 guibg=#370d15
hi DiffAdd         ctermfg=NONE ctermbg=NONE gui=NONE guifg=#dfdf00 guibg=#370d15

" Cursor
hi Cursor          ctermfg=NONE ctermbg=NONE gui=NONE guifg=#000000 guibg=#aaaaaa
hi CursorIM        ctermfg=NONE ctermbg=NONE gui=NONE guifg=#070707 guibg=#aaaaaa
hi CursorLine      ctermfg=NONE ctermbg=NONE gui=NONE guibg=#1c1c1c
hi CursorColumn    ctermfg=NONE ctermbg=NONE gui=NONE guibg=#0A0A0A

" Fold
hi Folded          ctermfg=NONE ctermbg=NONE gui=NONE guifg=#666666 guibg=#333333
hi FoldColumn      ctermfg=NONE ctermbg=NONE gui=NONE guifg=#444444 guibg=#1c1c1c

" Other
hi Directory       ctermfg=NONE ctermbg=NONE gui=NONE guifg=#aaaaba guibg=NONE
hi LineNr          ctermfg=NONE ctermbg=NONE gui=italic guifg=#444444 guibg=#1c1c1c 
hi NonText         ctermfg=NONE ctermbg=NONE gui=NONE guifg=#111111 guibg=#111111
hi SpecialKey      ctermfg=NONE ctermbg=NONE gui=NONE guifg=#378fff guibg=NONE
hi Title           ctermfg=NONE ctermbg=NONE gui=NONE guifg=#ffbf9f guibg=#370f07
hi Visual          ctermfg=NONE ctermbg=NONE gui=reverse guifg=#a5a5a5 guibg=#353535
hi SpellBad        term=reverse ctermbg=Red  gui=undercurl guisp=#aa1565

" Syntax group
hi Comment         ctermfg=NONE ctermbg=NONE gui=italic guifg=#777777 guibg=NONE
hi SpecialComment  ctermfg=NONE ctermbg=NONE gui=bold,italic guifg=#999999 guibg=NONE

hi Type            ctermfg=NONE ctermbg=NONE gui=NONE guifg=#778899 guibg=NONE
hi Statement       ctermfg=NONE ctermbg=NONE gui=NONE guifg=#778899 guibg=NONE
hi Constant        ctermfg=NONE ctermbg=NONE gui=NONE guifg=#99aabb guibg=#191919
hi Error           ctermfg=NONE ctermbg=NONE gui=NONE guifg=NONE    guibg=#ff0000
hi Identifier      ctermfg=NONE ctermbg=NONE gui=NONE guifg=#99aabb guibg=NONE
hi Ignore          ctermfg=NONE ctermbg=NONE gui=NONE guifg=NONE guibg=NONE
hi PreProc         ctermfg=NONE ctermbg=NONE gui=NONE guifg=#bbaa99 guibg=NONE
hi Special         ctermfg=NONE ctermbg=NONE gui=NONE guifg=#cccccc guibg=NONE
hi Todo            ctermfg=NONE ctermbg=NONE gui=NONE guifg=#ffff00 guibg=NONE
hi Underlined      ctermfg=NONE ctermbg=NONE gui=UNDERLINE guifg=NONE guibg=NONE

