" rainbow.vim : provides "rainbow-colored" curly braces and parentheses
" C language version
"   Author: 	Charles E. Campbell, Jr.
"   Date:		Sep 02, 2008
"   Version:	2c	ASTRO-ONLY
" ---------------------------------------------------------------------
" non-compatible only: {{{1
if &cp
 finish
endif
let keepcpo= &cpo
set cpo&vim

" ---------------------------------------------------------------------
" Default Settings: {{{1
let b:hlrainbow= "v2c"
if !exists("g:hlrainbow")
 let g:hlrainbow= "{}()"
endif

" ---------------------------------------------------------------------
" Clusters: {{{1
syn cluster cBracketGroup contains=cBlock,cBracket,cCharacter,cComment,cCommentError,cCommentL,cConditional,cConstant,cDefine,cErrInBracket,cInclude,cLabel,cMulti,cNumbers,cOperator,cParen,cPreProc,cRepeat,cSpaceError,cSpecialCharacter,cSpecialError,cStatement,cStorageClass,cString,cStructure,cType
syn cluster cCppBracketGroup contains=cBlock,cCharacter,cComment,cCommentError,cCommentL,cConditional,cConstant,cCppBracket,cCppParen,cCppString,cDefine,cErrInBracket,cInclude,cLabel,cMulti,cNumbers,cOperator,cPreProc,cRepeat,cSpaceError,cSpecialCharacter,cSpecialError,cStatement,cStorageClass,cStructure,cType

" ---------------------------------------------------------------------
" supports {} highlighting, too many } error detection, and {{{1
" function folding (when fdm=syntax)
if g:hlrainbow =~ "[{}]"
 " syntax: {{{2
 syn cluster cCurlyGroup contains=cConditional,cConstant,cLabel,cOperator,cRepeat,cStatement,cStorageClass,cStructure,cType,cBitField,cCharacter,cCommentError,cInclude,cNumbers,cParenError,cPreCondit,cSpaceError,cSpecialCharacter,cSpecialError,cUserCont,cBlock,cBracket,cComment,cCommentL,cCppOut,cCppString,cDefine,cMulti,cParen,cPreCondit,cPreProc,cString
 if &ft == "cpp"
  syn cluster cCurlyGroup add=cppStatement,cppAccess,cppType,cppExceptions,cppOperator,cppCast,cppStorageClass,cppStructure,cppNumber,cppBoolean,cppMinMax
 endif
 syn match  cCurlyError	display '}'
 syn region cCurly0 fold matchgroup=hlLevel0 start='{' end='}' 			 contains=@cCurlyGroup,cCurly1
 syn region cCurly1		 matchgroup=hlLevel1 start='{' end='}' contained contains=@cCurlyGroup,cCurly2
 syn region cCurly2		 matchgroup=hlLevel2 start='{' end='}' contained contains=@cCurlyGroup,cCurly3
 syn region cCurly3		 matchgroup=hlLevel3 start='{' end='}' contained contains=@cCurlyGroup,cCurly4
 syn region cCurly4		 matchgroup=hlLevel4 start='{' end='}' contained contains=@cCurlyGroup,cCurly5
 syn region cCurly5		 matchgroup=hlLevel5 start='{' end='}' contained contains=@cCurlyGroup,cCurly6
 syn region cCurly6		 matchgroup=hlLevel6 start='{' end='}' contained contains=@cCurlyGroup,cCurly7
 syn region cCurly7		 matchgroup=hlLevel7 start='{' end='}' contained contains=@cCurlyGroup,cCurly8
 syn region cCurly8		 matchgroup=hlLevel8 start='{' end='}' contained contains=@cCurlyGroup,cCurly9
 syn region cCurly9		 matchgroup=hlLevel9 start='{' end='}' contained contains=@cCurlyGroup,cCurly0
endif

" ---------------------------------------------------------------------
" supports () highlighting and error detection {{{1
if g:hlrainbow =~ "[()]"
 " syntax: {{{2
 syn cluster cParenGroup	contains=cBlock,cBracket,cCharacter,cComment,cCommentError,cCommentL,cConditional,cConstant,cDefine,cErrInParen,cInclude,cLabel,cMulti,cNumbers,cOperator,cPreProc,cRepeat,cSpaceError,cSpecialCharacter,cSpecialError,cStatement,cStorageClass,cString,cStructure,cType
 syn cluster cCppParenGroup	contains=cBlock,cCharacter,cComment,cCommentError,cCommentL,cConditional,cConstant,cCppBracket,cCppString,cDefine,cErrInParen,cInclude,cLabel,cMulti,cNumbers,cOperator,cPreProc,cRepeat,cSpaceError,cSpecialCharacter,cSpecialError,cStatement,cStorageClass,cStructure,cType
 syn clear cParen cCppParen cBracket cCppBracket
 syn region	cParen		transparent matchgroup=hlLevel0	start='(' end=')' contains=@cParenGroup,cParen1
 syn region	cParen1		transparent matchgroup=hlLevel1	start='(' end=')' contains=@cParenGroup,cParen2
 syn region	cParen2		transparent matchgroup=hlLevel2	start='(' end=')' contains=@cParenGroup,cParen3
 syn region	cParen3		transparent matchgroup=hlLevel3	start='(' end=')' contains=@cParenGroup,cParen4
 syn region	cParen4		transparent matchgroup=hlLevel4	start='(' end=')' contains=@cParenGroup,cParen5
 syn region	cParen5		transparent matchgroup=hlLevel5	start='(' end=')' contains=@cParenGroup,cParen6
 syn region	cParen6		transparent matchgroup=hlLevel6	start='(' end=')' contains=@cParenGroup,cParen7
 syn region	cParen7		transparent matchgroup=hlLevel7	start='(' end=')' contains=@cParenGroup,cParen8
 syn region	cParen8		transparent matchgroup=hlLevel8	start='(' end=')' contains=@cParenGroup,cParen9
 syn region	cParen9		transparent matchgroup=hlLevel9	start='(' end=')' contains=@cParenGroup,cParen

 syn region	cCppParen	transparent matchgroup=hlLevel0	start='(' skip='\\$' excludenl end=')' end='$' contained contains=@cCppParenGroup,cCppParen1 
 syn region	cCppParen1	transparent matchgroup=hlLevel1	start='(' skip='\\$' excludenl end=')' end='$' contained contains=@cCppParenGroup,cCppParen2 
 syn region	cCppParen2	transparent matchgroup=hlLevel2	start='(' skip='\\$' excludenl end=')' end='$' contained contains=@cCppParenGroup,cCppParen3 
 syn region	cCppParen3	transparent matchgroup=hlLevel3	start='(' skip='\\$' excludenl end=')' end='$' contained contains=@cCppParenGroup,cCppParen4 
 syn region	cCppParen4	transparent matchgroup=hlLevel4	start='(' skip='\\$' excludenl end=')' end='$' contained contains=@cCppParenGroup,cCppParen5 
 syn region	cCppParen5	transparent matchgroup=hlLevel5	start='(' skip='\\$' excludenl end=')' end='$' contained contains=@cCppParenGroup,cCppParen6 
 syn region	cCppParen6	transparent matchgroup=hlLevel6	start='(' skip='\\$' excludenl end=')' end='$' contained contains=@cCppParenGroup,cCppParen7 
 syn region	cCppParen7	transparent matchgroup=hlLevel7	start='(' skip='\\$' excludenl end=')' end='$' contained contains=@cCppParenGroup,cCppParen8 
 syn region	cCppParen8	transparent matchgroup=hlLevel8	start='(' skip='\\$' excludenl end=')' end='$' contained contains=@cCppParenGroup,cCppParen9 
 syn region	cCppParen9	transparent matchgroup=hlLevel9	start='(' skip='\\$' excludenl end=')' end='$' contained contains=@cCppParenGroup,cCppParen
 syn region	cBracket	transparent start='\[\|<::\@!' end=']\|:>' contains=@cBracketGroup
 syn region	cCppBracket	transparent start='\[\|<::\@!' skip='\\$' excludenl end=']\|:>' end='$' contained contains=@cCppBracketGroup
 " highlighting: {{{2
endif
 
" highlighting: {{{2
hi link cCurlyError	Error
if &bg == "dark"
 hi default   hlLevel0 ctermfg=red         guifg=red1
 hi default   hlLevel1 ctermfg=yellow      guifg=orange1      
 hi default   hlLevel2 ctermfg=green       guifg=yellow1      
 hi default   hlLevel3 ctermfg=cyan        guifg=greenyellow  
 hi default   hlLevel4 ctermfg=magenta     guifg=green1       
 hi default   hlLevel5 ctermfg=red         guifg=springgreen1 
 hi default   hlLevel6 ctermfg=yellow      guifg=cyan1        
 hi default   hlLevel7 ctermfg=green       guifg=slateblue1   
 hi default   hlLevel8 ctermfg=cyan        guifg=magenta1     
 hi default   hlLevel9 ctermfg=magenta     guifg=purple1
else
 hi default   hlLevel0 ctermfg=red         guifg=red3
 hi default   hlLevel1 ctermfg=darkyellow  guifg=orangered3
 hi default   hlLevel2 ctermfg=darkgreen   guifg=orange2
 hi default   hlLevel3 ctermfg=blue        guifg=yellow3
 hi default   hlLevel4 ctermfg=darkmagenta guifg=olivedrab4
 hi default   hlLevel5 ctermfg=red         guifg=green4
 hi default   hlLevel6 ctermfg=darkyellow  guifg=paleturquoise3
 hi default   hlLevel7 ctermfg=darkgreen   guifg=deepskyblue4
 hi default   hlLevel8 ctermfg=blue        guifg=darkslateblue
 hi default   hlLevel9 ctermfg=darkmagenta guifg=darkviolet
endif

let &cpo= keepcpo
" vim: fdm=marker ft=vim
