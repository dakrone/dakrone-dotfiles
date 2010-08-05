"map!  
map!  
" ---------------------------------------------------------------------------
" first the disabled features due to security concerns
set modelines=0                  " no modelines [http://www.guninski.com/vim1.html]
let g:secure_modelines_verbose=0 " securemodelines vimscript
let g:secure_modelines_modelines = 15 " 15 available modelines

" ---------------------------------------------------------------------------
" operational settings
syntax on
set ruler                     " show the line number on the bar
set more                      " use more prompt
set autoread                  " watch for file changes
set number                    " line numbers
set nohidden                  " close the buffer when I close a tab (I use tabs more than buffers)
set noautowrite               " don't automagically write on :next
set lazyredraw                " don't redraw when don't have to
set showmode                  " show the mode all the time
set showcmd                   " Show us the command we're typing
set nocompatible              " vim, not vi
set autoindent smartindent    " auto/smart indent
set expandtab                 " expand tabs to spaces (except java, see autocmd below)
set smarttab                  " tab and backspace are smart
set tabstop=6                 " 6 spaces
set shiftwidth=6              " shift width
set scrolloff=3               " keep at least 3 lines above/below
set sidescrolloff=5           " keep at least 5 lines left/right
set backspace=indent,eol,start  " backspace over all kinds of things
set showfulltag               " show full completion tags
set noerrorbells              " no error bells please
set linebreak                 " wrap at 'breakat' instead of last char
set tw=500                    " default textwidth is a max of 500
set cmdheight=2               " command line two lines high
set undolevels=500            " 500 undos
set updatecount=100           " switch every 100 chars
set complete=.,w,b,u,U,t,i,d  " do lots of scanning on tab completion
set ttyfast                   " we have a fast terminal
filetype on                   " Enable filetype detection
filetype indent on            " Enable filetype-specific indenting
filetype plugin on            " Enable filetype-specific plugins
compiler ruby                 " Enable compiler support for ruby
set wildmode=longest:full     " *wild* mode
set wildignore+=*.o,*~,.lo    " ignore object files
set wildmenu                  " menu has tab completion
let maplocalleader=','        " all my macros start with ,
" Deprecated, using SimpleFold with '\f' now. ,sf to revert
"set foldmethod=syntax         " fold on syntax automagically, always
set foldcolumn=4              " 4 lines of column for fold showing, always
set whichwrap+=<,>,h,l        " backspaces and cursor keys wrap to
set magic                     " Enable the "magic"
set visualbell t_vb=          " Disable ALL bells
set cursorline                " show the cursor line
set matchpairs+=<:>           " add < and > to match pairs
"set tags=tags;/               " search recursively up for tags
set tags=~/.vtags,tags        " tag filenames

" disable yankring for now
let g:loaded_yankring = 1
" disable paredit for now
let g:paredit_mode = 0

" Use 'par' (sudo port install par) to format paragraphs with a width of 80
set formatprg=par\ -w80

" highlight over 80 columns
"highlight OverLength ctermbg=darkred ctermfg=white guibg=#FFD9D9
highlight OverLength cterm=reverse
match OverLength /\%81v.*/

set dictionary=/usr/share/dict/words " more words!

if !has("gui_running")
      "colorscheme candycode   " yum candy

      " I pretty much only like this scheme if I can use SIMBL with terminal
      " colors:
      " (http://www.culater.net/software/TerminalColors/TerminalColors.php)
      " to change the really hard-to-read dark blue into a lighter shade.
      " Or; Use iterm with Tango colors
      "colorscheme Mustang
      "colorscheme rdark
      colorscheme ir_black_new
end
if has("gui_running")
      colorscheme rdark
      let rdark_current_line=1  " highlight current line
      set background=dark
      set noantialias
      set guioptions-=T        " no toolbar
      set guioptions-=l        " no left scrollbar
      set guioptions-=L        " no left scrollbar
      set guioptions-=r        " no right scrollbar
      set guioptions-=R        " no right scrollbar
      set lines=64
      set columns=135
      set transparency=0
      set gfn=Monaco:h9.0
      set clipboard=unnamed
end

if exists('&t_SI')
      let &t_SI = "\<Esc>]12;lightgoldenrod\x7"
      let &t_EI = "\<Esc>]12;grey80\x7"
endif
      
" fish fixes
if $SHELL =~ 'bin/fish'
      set shell=/usr/bin/zsh
endif


" Settings for NERD_Tree
let NERDTreeWinPos="left"
let NERDTreeWinSize=35

" Settings for taglist.vim
let Tlist_Use_Right_Window=1
let Tlist_Auto_Open=0
let Tlist_Enable_Fold_Column=0
let Tlist_Show_One_File = 1         " Only show tags for current buffer
let Tlist_Compact_Format=0
let Tlist_WinWidth=28
let Tlist_Exit_OnlyWindow=1
let Tlist_File_Fold_Auto_Close = 1

" Settings for :TOhtml
let html_number_lines=1
let html_use_css=1
let use_xhtml=1

" Settings for VimClojure
let g:vimclojure#HighlightBuiltins=1   " Highlight Clojure's builtins
let g:vimclojure#ParenRainbow=1        " Rainbow parentheses'!
let g:vimclojure#DynamicHighlighting=1 " Dynamically highlight functions
let vimclojure#NailgunClient="/Users/hinmanm/bin/ng" " Nailgun location
let vimclojure#WantNailgun=1
let vimclojure#SplitPos = "right"
"let g:clj_want_gorilla=1            " Bananas! (Make sure nailgun in is your path)

" Settings for yankring
let g:yankring_history_dir="/Users/hinmanm/.vim/"
let g:yankring_history_file="yank.txt"

" Settings for twitvim
let twitvim_login=''                " Requires using ,ts to input your username/password
let g:twitvim_enable_python=1       " Use python for fetchinng the tweets
let g:twitvim_count=30              " Grab 30 tweets
" Use SSL for twitter
let twitvim_api_root = "https://twitter.com"
map <LocalLeader>tf :FriendsTwitter<cr>
map <LocalLeader>ts :let twitvim_login=inputdialog('Twitter USER:PASS? ')<cr>
map <LocalLeader>tw :PosttoTwitter<cr>

" Bindings for Narrow/Widen
map <LocalLeader>N :Narrow<cr>
map <LocalLeader>W :Widen<cr>

" Bindings for ctk
nnoremap <LocalLeader>C :CC<cr>
" I don't like having xterm open.
let g:ctk_execprg=''

" PHP settings
let php_sql_query=1
let php_htmlInStrings=1
let php_noShortTags=1
let php_folding=1

" Ri settings
let ri_split_orientation='vertical'
let ri_prompt_complete='on'

" Supertab settings
" supertab + eclim == java win
let g:SuperTabDefaultCompletionType = "context"
" Omni for vimclojure
autocmd FileType clojure let g:SuperTabDefaultCompletionType = "<c-x><c-o>"
let g:SuperTabDefaultCompletionTypeDiscovery = [
                  \ "&completefunc:<c-x><c-u>",
                  \ "&omnifunc:<c-x><c-o>",
                  \ ]
let g:SuperTabLongestHighlight = 1

" Gist.vim settings (http://www.vim.org/scripts/script.php?script_id=2423)
let g:gist_open_browser_after_post = 1
" Mac-specific
let g:gist_browser_command = 'open %URL%'
let g:gist_clip_command = 'pbcopy'

" SimpleFold settings
" This doesn't work yet.
let g:clojure_simplefold_nestable_start_expr = '\v\(defn'
let g:clojure_simplefold_nestable_end_expr = '\v^\s*$'

" Remap up and down arrow in VimClojure REPL
"function CustomiseClojureRepl()
  "if exists("b:vimclojure_repl")
    "inoremap <buffer> <Up> <Plug>ClojureReplUpHistory
    "inoremap <buffer> <Down> <Plug>ClojureReplDownHistory
  "endif
"endfunction
"autocmd FileType clojure call CustomiseClojureRepl()

" Showmarks settings
let g:showmarks_ignore_type="hmprq"
let g:showmarks_enable=0

" Changes.vim settings
let g:changes_autocmd=1      " Autoshow changes on cursorpause
let g:changes_verbose=0      " Don't show what symbols mean

" Eclim settings
" ,i imports whatever is needed for current line
nnoremap <silent> <LocalLeader>i :JavaImport<cr>
" ,d opens javadoc for statement in browser
nnoremap <silent> <LocalLeader>d :JavaDocSearch -x declarations<cr>
" ,<enter> searches context for statement
nnoremap <silent> <LocalLeader><cr> :JavaSearchContext<cr>
" ,jv validates current java file
nnoremap <silent> <LocalLeader>jv :Validate<cr>
" ,jc shows corrections for the current line of java
nnoremap <silent> <LocalLeader>jc :JavaCorrect<cr>
" 'open' on OSX will open the url in the default browser without issue
let g:EclimBrowser='open'
" Disable Eclim's taglisttoo because I use the regular taglist plugin
"let g:taglisttoo_disabled = 1


" ---------------------------------------------------------------------------
"  configuration for fuzzyfinder
" find in buffer is ,b
nmap <LocalLeader>b :FuzzyFinderBuffer<CR>
" see: http://github.com/viperinthought/fuzzyfinder_textmate/
" FuzzyFinderTextMate is broken at the moment due to vim patches
"nmap <LocalLeader>f :FuzzyFinderTextMate<CR>
nmap <LocalLeader>f :FuzzyFinderFile<CR>
" find in file is ,F
nmap <LocalLeader>F :FuzzyFinderFile<CR>
" find in tag is ,t
nmap <LocalLeader>t :FuzzyFinderTag<CR>



" ---------------------------------------------------------------------------
"  configuration for localvimrc.vim
" don't ask, just source
let g:localvimrc_ask=0
" only source a max of 2 files
let g:localvimrc_count=2


" ---------------------------------------------------------------------------
"  config for easytags
let g:easytags_file = '~/.vtags'
" tag-related keybinds:
" open tag in new tab
map <C-\> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>
" open tag in split with ,\
map <LocalLeader>\ :split <CR>:exec("tag ".expand("<cword>"))<CR>
" open tag in vsplit with ,]
map <LocalLeader>] :vsplit <CR>:exec("tag ".expand("<cword>"))<CR>


" ---------------------------------------------------------------------------
" status line 
set laststatus=2
if has('statusline')
      " Status line detail: (from Rafael Garcia-Suarez)
      " %f		file path
      " %y		file type between braces (if defined)
      " %([%R%M]%)	read-only, modified and modifiable flags between braces
      " %{'!'[&ff=='default_file_format']}
      "			shows a '!' if the file format is not the platform
      "			default
      " %{'$'[!&list]}	shows a '*' if in list mode
      " %{'~'[&pm=='']}	shows a '~' if in patchmode
      " (%{synIDattr(synID(line('.'),col('.'),0),'name')})
      "			only for debug : display the current syntax item name
      " %=		right-align following items
      " #%n		buffer number
      " %l/%L,%c%V	line number, total number of lines, and column number

      function! SetStatusLineStyle()
            "let &stl="%f %y "                       .
            "\"%([%R%M]%)"                   .
            "\"%#StatusLineNC#%{&ff=='unix'?'':&ff.'\ format'}%*" .
            "\"%{'$'[!&list]}"               .
            "\"%{'~'[&pm=='']}"              .
            "\"%="                           .
            "\"#%n %l/%L,%c%V "              .
            "\""
            "      \"%#StatusLineNC#%{GitBranchInfoString()}%* " .
            let &stl="%F%m%r%h%w\ [%{&ff}]\ [%Y]\ %P\ %=[a=\%03.3b]\ [h=\%02.2B]\ [%l,%v]"
      endfunc
      " Not using it at the moment, using a different one
      call SetStatusLineStyle()

      if has('title')
            set titlestring=%t%(\ [%R%M]%)
      endif

      "highlight StatusLine    ctermfg=White ctermbg=DarkBlue cterm=bold
      "highlight StatusLineNC  ctermfg=White ctermbg=DarkBlue cterm=NONE
endif

" ---------------------------------------------------------------------------
"  searching
set incsearch                 " incremental search
set ignorecase                " search ignoring case
set smartcase                 " Ignore case when searching lowercase
set hlsearch                  " highlight the search
set showmatch                 " show matching bracket
set diffopt=filler,iwhite     " ignore all whitespace and sync

" ---------------------------------------------------------------------------
"  mouse stuffs
"set mouse=a                   " mouse support in all modes
set mousehide                 " hide the mouse when typing
" this makes the mouse paste a block of text without formatting it 
" (good for code)
map <MouseMiddle> <esc>"*p

" ---------------------------------------------------------------------------
"  backup options
set backup
set backupdir=~/.backup
set viminfo=%100,'100,/100,h,\"500,:100,n~/.viminfo
set history=200
"set viminfo='100,f1

" ---------------------------------------------------------------------------
" spelling...
if v:version >= 700

      setlocal spell spelllang=en
      nmap <LocalLeader>ss :set spell!<CR>

endif
" default to no spelling
set nospell

" ---------------------------------------------------------------------------
" Turn on omni-completion for the appropriate file types.
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags
autocmd FileType php set omnifunc=phpcomplete#CompletePHP
autocmd FileType c set omnifunc=ccomplete#Complete
autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1  " Rails support
autocmd FileType java setlocal noexpandtab " don't expand tabs to spaces for Java
" put these lines in ~/.vim/.compiler_info
"au FileType ruby let b:{g:ctk_ext_var} = 'rb'
"au FileType ruby SetCompilerInfo ruby title='Ruby 1.9.1 - Matz' cmd='ruby $flags $input' run='ruby $input' flags='-wc' debug_flags='-rdebug $flags'

" I want Ri access *all* the time.
source ~/.vim/ftplugin/ri.vim


" ---------------------------------------------------------------------------
" some useful mappings
" Omnicomplete as Ctrl+Space
inoremap <Nul> <C-x><C-o>
" Also map user-defined omnicompletion as Ctrl+k
inoremap <C-k> <C-x><C-u>
" Y yanks from cursor to $
map Y y$
" for yankring to work with previous mapping:
function! YRRunAfterMaps()
      nnoremap Y   :<C-U>YRYankCount 'y$'<CR>
endfunction
" toggle list mode
nmap <LocalLeader>tl :set list!<cr>
" Use the same symbols as TextMate for tabstops and EOLs
"set listchars=tab:▸\ ,eol:¬,trail:-
set listchars=eol:\ ,tab:»-,trail:·,precedes:…,extends:…,nbsp:‗
" toggle paste mode
nmap <LocalLeader>pm :set paste!<cr>
" toggle wrapping
nmap <LocalLeader>ww :set wrap!<cr>
" change directory to that of current file
nmap <LocalLeader>cd :cd%:p:h<cr>
" change local directory to that of current file
nmap <LocalLeader>lcd :lcd%:p:h<cr>
" correct type-o's on exit
nmap q: :q
" save and build
nmap <LocalLeader>wm  :w<cr>:make<cr>
" open all folds
nmap <LocalLeader>o  :%foldopen!<cr>
" close all folds
nmap <LocalLeader>c  :%foldclose!<cr>
" ,tt will toggle taglist on and off
nmap <LocalLeader>tt :Tlist<cr>
" ,nn will toggle NERDTree on and off
nmap <LocalLeader>n :NERDTreeToggle<cr>
" When I'm pretty sure that the first suggestion is correct
map <LocalLeader>st 1z=
" q: sucks
nmap q: :q
" If I forgot to sudo vim a file, do that with :w!!
cmap w!! %!sudo tee > /dev/null %
" Fix the # at the start of the line
inoremap # X<BS>#
" When I forget I'm in Insert mode, how often do you type 'jj' anyway?:
imap jj <Esc>
" Fold with paren begin/end matching
nmap F zf%
" When I use ,sf - return to syntax folding with a big foldcolumn
nmap <LocalLeader>sf :set foldcolumn=6 foldmethod=syntax<cr>
" ruby helpers
iab rbang #!/usr/bin/env ruby -w<cr> encoding: UTF-8<cr>
" For mac users (using the 'apple' key)
"map <D-S-]> gt " MacVim already does this
"map <D-S-[> gT " MacVim already does this
map <D-1> 1gt
map <D-2> 2gt
map <D-3> 3gt
map <D-4> 4gt
map <D-5> 5gt
map <D-6> 6gt
map <D-7> 7gt
map <D-8> 8gt
map <D-9> 9gt
map <D-0> :tablast<CR>
map <C-p> :tabprev<CR>
map <C-n> :tabnext<CR>
 
" Command + movement for wrapped lines.
vmap <D-j> gj
vmap <D-k> gk
"vmap <D-4> g$
vmap <D-6> g^
vmap <D-0> g^

" for linux and windows users (using the control key)
"map <C-S-]> gt
"map <C-S-[> gT
"map <C-1> 1gt
"map <C-2> 2gt
"map <C-3> 3gt
"map <C-4> 4gt
"map <C-5> 5gt
"map <C-6> 6gt
"map <C-7> 7gt
"map <C-8> 8gt
"map <C-9> 9gt
"map <C-0> :tablast<CR>

" Vimperator-like bindings for a tab switches.
nmap b1 1gt
nmap b2 2gt
nmap b3 3gt
nmap b4 4gt
nmap b5 5gt
nmap b6 6gt
nmap b7 7gt
nmap b8 8gt
nmap b9 9gt

" Compile Ruby code after writing (show warnings/errors)
function! Compile()
  " don't compile if it's an Rspec file (extra warnings)
  let name = expand('<afile>')
  if name !~ 'spec'
    CC
  endif
endfunction
autocmd BufWritePost *.rb call Compile()

" Diff with saved version of the file
function! s:DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
com! DiffSaved call s:DiffWithSaved()


" ---------------------------------------------------------------------------
"  xmpfilter ruby stuff
"  `gem install rcodetools`

" Remove the '# => ' lines that xmpfilter adds
function! RemoveRubyEval() range
  let begv = a:firstline
  let endv = a:lastline
  normal Hmt
  set lz
  execute ":" . begv . "," . endv . 's/\s*# \(=>\|!!\).*$//e'
  normal 'tzt`s
  set nolz
  redraw
endfunction

" Add # => markers
" ,m for "Add mark"
vmap <silent> <LocalLeader>m !xmpfilter -m<cr>
nmap <silent> <LocalLeader>m V<LocalLeader>e

" Remove # => markers
" ,r for "Remove mark"
vmap <silent> <LocalLeader>r ms:call RemoveRubyEval()<CR>
nmap <silent> <LocalLeader>r V<LocalLeader>r

" plain annotations
" ,e for "Eval marks"
map <silent> <LocalLeader>e !xmpfilter -a<cr>
nmap <silent> <LocalLeader>e V<LocalLeader>e

" Test::Unit assertions; use -s to generate RSpec expectations instead
" ,s for "Spec generation"
map <silent> <LocalLeader>s !xmpfilter -s<cr>
nmap <silent> <LocalLeader>s V<LocalLeader>s

" Annotate the full buffer
" I actually prefer ggVG to %; it's a sort of poor man's visual bell 
" ,fe for "Full Eval"
nmap <silent> <LocalLeader>Fe mzggVG!xmpfilter -a<cr>'z

" assertions
nmap <silent> <S-F11> mzggVG!xmpfilter -u<cr>'z

" Ack helpers
" replaced by this vimscript: http://www.vim.org/scripts/script.php?script_id=2572



" ---------------------------------------------------------------------------
" setup for the visual environment
if $TERM =~ '^xterm'
      set t_Co=256 
elseif $TERM =~ '^screen-bce'
      set t_Co=256            " just guessing
elseif $TERM =~ '^rxvt'
      set t_Co=88
elseif $TERM =~ '^linux'
      set t_Co=8
else
      set t_Co=16
endif


" ---------------------------------------------------------------------------
" tabs
" (LocalLeader is ",")
map <LocalLeader>tc :tabnew %<cr>    " create a new tab       
map <LocalLeader>td :tabclose<cr>    " close a tab
map <LocalLeader>tn :tabnext<cr>     " next tab
map <silent><m-Right> :tabnext<cr>           " next tab
map <LocalLeader>tp :tabprev<cr>     " previous tab
map <silent><m-Left> :tabprev<cr>            " previous tab
map <LocalLeader>tm :tabmove         " move a tab to a new location

" ---------------------------------------------------------------------------
" auto load extensions for different file types
if has('autocmd')
      filetype plugin indent on
      syntax on

      " jump to last line edited in a given file (based on .viminfo)
      "autocmd BufReadPost *
      "       \ if !&diff && line("'\"") > 0 && line("'\"") <= line("$") |
      "       \       exe "normal g`\"" |
      "       \ endif
      autocmd BufReadPost *
                        \ if line("'\"") > 0|
                        \       if line("'\"") <= line("$")|
                        \               exe("norm '\"")|
                        \       else|
                        \               exe "norm $"|
                        \       endif|
                        \ endif

      " improve legibility
      au BufRead quickfix setlocal nobuflisted wrap number

      " configure various extensions
      let git_diff_spawn_mode=2

      " improved formatting for markdown
      " http://plasticboy.com/markdown-vim-mode/
      autocmd BufRead *.mkd  set ai formatoptions=tcroqn2 comments=n:>
endif
