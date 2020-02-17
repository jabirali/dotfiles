"------------------------------------------------------------
" File: Neovim config
" Path: ~/.config/nvim/init.vim
"------------------------------------------------------------

" Plugin manager
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

" Plugin install
call plug#begin('~/.local/share/nvim/plugins')
  Plug 'Brettm12345/moonlight.vim'
  Plug 'lervag/vimtex'
  Plug 'plasticboy/vim-markdown'
  Plug 'jceb/vim-orgmode'
call plug#end()

" Color scheme
let g:moonlight_terminal_italics=1
set termguicolors
set background=dark
colorscheme moonlight

" User interface
set scrolloff=5 sidescrolloff=5 nowrap
set noshowmode
set hidden
set number
set ruler

" Spell check
set nospell
set spelllang=en
set spellcapcheck=off

" Search
set ignorecase
set smartcase
set incsearch
set hlsearch

" Indentation
set tabstop=4
set shiftwidth=4

" Code folding
set foldenable
set foldlevelstart=0
set foldnestmax=3
set foldmethod=indent

" Miscellaneous
set encoding=utf-8
set virtualedit=block
set clipboard=unnamedplus
set tildeop
set autochdir

" Keybindings
let mapleader="\<space>"
noremap Y y$
