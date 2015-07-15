" ------------------------------------------------------------------------------
" Filename:      ~/.vim/colors/relaxedgreen.vim
" Last Modified: 24 Apr 2004 23:11:17 by Dave Vehrs
" Maintainer:    Dave Vehrs (davev at ziplip.com)
" Copyright:     2002-2003 Dave Vehrs
"                This script is free software; you can redistribute it and/or 
"                modify it under the terms of the GNU General Public License as 
"                published by the Free Software Foundation; either version 2 of 
"                the License, or (at your option) any later version. 
" Description:   Vim colorscheme file.
" Install:       Put this file in the users colors directory (~/.vim/colors) or 
"                in the shared colors directory (/usr/shared/vim/vim61/colors/),
"                then load it with :colorscheme relaxedgreen
" ------------------------------------------------------------------------------
set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "relaxedgreen"
hi Comment		term=italic			ctermfg=darkcyan			    guifg=#00a594
hi Constant		term=underline		ctermfg=blue			        guifg=#0099dd
hi DiffAdd		term=reverse		ctermfg=black ctermbg=cyan	    guifg=#000000 guibg=#007200
hi DiffChange 	term=underline		cterm=reverse ctermfg=darkgreen ctermbg=black guifg=#000000 guibg=#006700
hi DiffDelete 	term=standout	    ctermfg=black ctermbg=cyan      guifg=#000000 guibg=#007200
hi DiffText		term=bold			ctermfg=green ctermbg=black     guifg=#00ac00 guibg=#000000
hi Error		term=reverse,bold 	ctermfg=black ctermbg=red 	    guifg=#000000 guibg=#dc0000
hi Folded                       	ctermfg=darkgreen ctermbg=black guifg=#20de20 guibg=#000000
hi Function		term=standout		ctermfg=darkgreen	 			guifg=#22bb22
hi Identifier 	term=underline		ctermfg=darkcyan				guifg=#0044cc
hi Ignore							ctermfg=lightgreen			    guifg=#336633
hi IncSearch 	term=reverse 		cterm=reverse 				    gui=reverse
hi LineNr       term=bold           ctermfg=green                   guifg=#00ff00
hi ModeMsg 		term=bold 			cterm=bold 					    gui=bold
hi Normal		                	ctermfg=gray ctermbg=black	    guifg=#aaaaaa guibg=#000000
hi NonText							ctermfg=darkcyan				guifg=#999999
hi PreProc	    term=standout		ctermfg=darkgreen	 			guifg=#22bb22
hi Question     term=standout       ctermfg=red                     guifg=#ff0000
hi Special		term=bold			ctermfg=green	    			guifg=#00ff00
hi Statement	term=standout   	ctermfg=darkred 				gui=none guifg=#ac0000
hi StatusLine	term=reverse 		cterm=reverse ctermfg=darkgreen ctermbg=black gui=none guibg=#228822 guifg=#333333
hi StatusLineNC term=reverse 		cterm=reverse ctermfg=darkgreen ctermbg=darkblue gui=none guibg=#336633 guifg=#449988
hi Title        term=reverse        ctermfg=black ctermbg=green     guifg=#000000 guibg=#00ff00
hi Type			term=standout		ctermfg=green				    guifg=#559955
hi Visual       term=reverse        cterm=none ctermfg=black ctermbg=darkgreen gui=none guifg=#00d400 guibg=#007400
hi WildMenu     term=reverse		ctermfg=black ctermbg=darkgreen guifg=#000000 guibg=#00ac00

" ------------------------------------------------------------------------------
" Common groups that link to other highlighting definitions.

hi link Character		Constant
hi link Number			Constant
hi link Boolean			Constant
hi link String			Constant

hi link FoldColumn      Folded

hi link Search			IncSearch

hi link Debug			Error
hi link ErrorMsg    	Error
hi link WarningMsg		Error
hi link Todo        	Error

hi link Operator		LineNr

hi link Float			Number

hi link Define			PreProc
hi link Include			PreProc
hi link Macro			PreProc
hi link PreCondit		PreProc

hi link Repeat			Question

hi link Conditional		Repeat

hi link Delimiter		Special
hi link SpecialChar		Special
hi link SpecialComment 	Special
hi link SpecialKey		Special
hi link Tag				Special

hi link Exception		Statement
hi link Keyword			Statement
hi link Label			Statement

hi link StorageClass	Type
hi link Structure		Type
hi link Typedef			Type

hi link VisualNOS       Visual

hi link VertSplit    	WildMenu
" ------------------------------------------------------------------------------
" vim: tw=0 ts=4 sw=4
