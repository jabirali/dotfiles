"set autochdir
"set autoshelldir
set formatprg=fmt
set updatetime=300
set nobackup
set nowritebackup

set shortmess+=I
set nonumber
set signcolumn=no
set noruler
set noshowmode
set noshowcmd
set scrolloff=0
set hidden

set splitbelow
set splitright
set statusline=\ %<%f%=%P\ 
"set fillchars+=fold:\ ,eob:\ ,vert:\ ,
set path+=**

set tabstop=4
set softtabstop=4
set shiftwidth=4

set nowrap
set linebreak
set breakindent
set breakindentopt=list:-1
set smoothscroll

set smartcase
set ignorecase
set incsearch

set autoread
set clipboard=unnamed
set virtualedit=block
set wildoptions=pum
set mouse=a
set tildeop

set guioptions=gme
set guicursor+=a:blinkon0
set guifont=JetBrains\ Mono\ NL:h14

nnoremap <space> <nop>
let mapleader=' '
let maplocalleader=','

" set fillchars+=vert:│,
set fillchars+=vert:\ ,

" CleverF
map ; :

" Change cursor between modes.
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"



nnoremap \ <cmd>Lexplore<cr>
let g:netrw_banner=0
" let g:netrw_keepdir=0

nnoremap <leader>3 <cmd>silent Lexplore scp://ex3/<cr>
