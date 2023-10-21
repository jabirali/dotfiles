" ~/.vim/vimrc

" This is a `medium heavy' vimrc. It doesn't have enough plugins setup that
" it's useful for working on large programming projects, but does have just
" enough enabled to make it useful for medium-long edits from the terminal.

" Sensible defaults (Vim 8+).
source $VIMRUNTIME/defaults.vim
packadd! matchit

set number
set nowrap
set noshowmode
set splitbelow
set splitright
set statusline=\ %<%f%=%P\ 
set fillchars=fold:\ ,vert:\ ,

set tabstop=4
set softtabstop=4
set shiftwidth=4

set smartcase
set ignorecase
set incsearch

set autoread
set termguicolors
set clipboard=unnamed
set virtualedit="block"

" Never pollute my workspace.
set undofile
set undodir=~/.vim/undo
set backupdir=~/.vim/back
set directory=~/.vim/swap
set viminfofile=~/.vim/info
let g:gutentags_cache_dir="~/.vim/tags"

" Bootstrap plugin manager.
if empty(glob('~/.vim/autoload/plug.vim'))
  silent execute '!curl -fLo ~/.vim/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Install and load plugins.
call plug#begin('~/.cache/vim/plug')
	Plug 'tpope/vim-rsi'
	Plug 'wellle/targets.vim'
	Plug 'machakann/vim-sandwich'
	Plug 'junegunn/vim-slash'
	Plug 'bronson/vim-visual-star-search'
	Plug 'tpope/vim-unimpaired'
	Plug 'tpope/vim-commentary'
	Plug 'ludovicchabant/vim-gutentags'
	Plug 'drewtempelmeyer/palenight.vim'
call plug#end()

" Customize colorscheme.
set background=dark
colorscheme palenight

highlight StatusLine guibg='#4f5473'
highlight StatusLineNC guibg='#383d53'
highlight! link StatusLineTerm StatusLine 
highlight! link StatusLineTermNC StatusLineNC
highlight! link Folded TabLine
