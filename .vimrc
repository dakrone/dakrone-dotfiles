"map!  
map!  
" ---------------------------------------------------------------------------
" first the disabled features due to security concerns
set modelines=0               " no modelines [http://www.guninski.com/vim1.html]
let g:secure_modelines_verbose=0 " securemodelines vimscript
let g:secure_modelines_modelines = 15 " 15 available modelines

" ---------------------------------------------------------------------------
" operational settings
syntax on
set ruler                     " show the line number on the bar
set more                      " use more prompt
set autoread                  " watch for file changes
set number                    " line numbers
set hidden                    " allow edit buffers to be hidden
set noautowrite               " don't automagically write on :next
set lazyredraw                " don't redraw when don't have to
set showmode
set showcmd                   " Show us the command we're typing
set nocompatible              " vim, not vi
set autoindent smartindent    " auto/smart indent
set expandtab                 " expand tabs to spaces
set smarttab                  " tab and backspace are smart
set tabstop=6                 " 6 spaces
set shiftwidth=6
set scrolloff=3               " keep at least 3 lines above/below
set sidescrolloff=5           " keep at least 5 lines left/right
set backspace=indent,eol,start
set showfulltag               " show full completion tags
set noerrorbells              " no error bells please
set linebreak
set tw=500                    " default textwidth is a max of 500
set cmdheight=2               " command line two lines high
set undolevels=1000           " 1000 undos
set updatecount=100           " switch every 100 chars
set complete=.,w,b,u,U,t,i,d  " do lots of scanning on tab completion
set ttyfast                   " we have a fast terminal
filetype on                   " Enable filetype detection
filetype indent on            " Enable filetype-specific indenting
filetype plugin on            " Enable filetype-specific plugins
compiler ruby                 " Enable compiler support for ruby
set wildmode=longest:full
set wildignore+=*.o,*~,.lo    " ignore object files
set wildmenu                  " menu has tab completion
let maplocalleader=','        " all my macros start with ,
set foldmethod=syntax         " fold on syntax automagically, always
set foldcolumn=5              " 3 lines of column for fold showing, always
set whichwrap+=<,>,h,l        " backspaces and cursor keys wrap to
set magic                     " Enable the "magic"
set visualbell t_vb=          " Disable ALL bells
set cursorline                " show the cursor line
set matchpairs+=<:>           " add < and > to match pairs
set tags=tags;/               " search recursively up for tags

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
      colorscheme ir_black_new " only when I can change certain colors
end
if has("gui_running")
      "colorscheme macvim      " macvim == win
      colorscheme ir_black_new     " only when I can change certain colors
      set noantialias          " If I use ir_black_new, no antialiasing
      set guioptions-=T        " no toolbar
      set lines=65
      set columns=120
      set gfn=Monaco:h9       " Monaco, 9pt font
end

if exists('&t_SI')
      let &t_SI = "\<Esc>]12;lightgoldenrod\x7"
      let &t_EI = "\<Esc>]12;grey80\x7"
endif
      
" fish fixes
if $SHELL =~ 'bin/fish'
      set shell=/usr/bin/zsh
endif


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
let g:clj_highlight_builtins=1      " Highlight Clojure's builtins
let g:clj_paren_rainbow=1           " Rainbow parentheses'!
let g:clj_want_gorilla=1            " Bananas! (Make sure nailgun in is your path)

" Settings for yankring
let g:yankring_histotry_dir="~/.vim/"
let g:yankring_histotry_file="~/.vim/yank.txt"

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
let g:SuperTabDefaultCompletionTypeDiscovery = [
                  \ "&completefunc:<c-x><c-u>",
                  \ "&omnifunc:<c-x><c-o>",
                  \ ]
let g:SuperTabLongestHighlight = 1

" Eclim settings
" ,i imports whatever is needed for current line
nnoremap <silent> <buffer> <LocalLeader>i :JavaImport<cr>
" ,d opens javadoc for statement in browser
nnoremap <silent> <buffer> <LocalLeader>d :JavaDocSearch -x declarations<cr>
" ,<enter> searches context for statement
nnoremap <silent> <buffer> <LocalLeader><cr> :JavaSearchContext<cr>
" ,jv validates current java file
nnoremap <silent> <buffer> <LocalLeader>jv :Validate<cr>
" ,jc shows corrections for the current line of java
nnoremap <silent> <buffer> <LocalLeader>jc :JavaCorrect<cr>
" 'open' on OSX will open the url in the default browser without issue
let g:EclimBrowser='open'

" ---------------------------------------------------------------------------
"  configure autoclose
"  default to off, I'll turn it on if I want to
let g:AutoCloseOn = 0
"  default: {'(': ')', '{': '}', '[': ']', '"': '"', "'": "'"}
"  but completing ' makes typing lisp really suck, so I take it out of the defaults
let g:AutoClosePairs = {'(': ')', '{': '}', '[': ']', '"': '"'}


" ---------------------------------------------------------------------------
"  configuration for fuzzyfinder
" find in buffer is ,fb
nmap <LocalLeader>fb :FuzzyFinderBuffer<CR>
" find in file is ,ff
nmap <LocalLeader>ff :FuzzyFinderFile<CR>
" find in tag is ,ft
nmap <LocalLeader>ft :FuzzyFinderTag<CR>


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
set diffopt=filler,iwhite       " ignore all whitespace and sync

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
" toggle paste mode
nmap <LocalLeader>pp :set paste!<cr>
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
nmap <LocalLeader>fo  :%foldopen!<cr>
" close all folds
nmap <LocalLeader>fc  :%foldclose!<cr>
" ,tt will toggle taglist on and off
nmap <LocalLeader>tt :Tlist<cr>
" ,nn will toggle NERDTree on and off
nmap <LocalLeader>nn :NERDTreeToggle<cr>
" When I'm pretty sure that the first suggestion is correct
map <LocalLeader>r 1z=
" q: sucks
nmap q: :q
" If I forgot to sudo vim a file, do that with :w!!
cmap w!! %!sudo tee > /dev/null %
" Fix the # at the start of the line
inoremap # X<BS>#
" When I forget I'm in Insert mode, how often do you type 'jj' anyway?:
imap jj <Esc>
" ruby helpers
iab rbang #!/usr/bin/env ruby<cr>
iab idef def initialize

" Ack helpers
function! Ack(args)
      let grepprg_bak=&grepprg
      set grepprg=ack\ -H\ --nocolor\ --nogroup
      execute "silent! grep " . a:args
      botright copen
      let &grepprg=grepprg_bak
endfunction

command! -nargs=* -complete=file Ack call Ack(<q-args>)


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
map <LocalLeader>n :tabnext<cr>     " next tab
map <silent><m-Right> :tabnext<cr>           " next tab
map <LocalLeader>tp :tabprev<cr>     " previous tab
map <LocalLeader>p :tabprev<cr>     " previous tab
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
