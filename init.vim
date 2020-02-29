" vim:foldmethod=marker

"------------------------------------------------------------
" File: Neovim config
" Path: ~/.config/nvim/init.vim
"------------------------------------------------------------

" Plugins {{{ 
" Plugin manager
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

" Plugin install
call plug#begin('~/.local/share/nvim/plugins')
  " User interface
  Plug 'Brettm12345/moonlight.vim' " Moonlight colors
  Plug 'Konfekt/FastFold'          " Optimized folding
  " Text editing
  Plug 'andymass/vim-matchup'      " Smarter `%` jumps
  Plug 'junegunn/vim-slash'        " Smarter `*` jumps
  " Language support
  Plug 'lervag/vimtex'
  Plug 'jceb/vim-orgmode'
  Plug 'plasticboy/vim-markdown'
call plug#end()

" }}}

augroup matchup_matchparen_highlight
  autocmd!
  autocmd ColorScheme * hi MatchParen ctermfg=red cterm=none guifg=red gui=none
augroup END



" Color scheme
"let g:moonlight_terminal_italics=1
set termguicolors
set background=dark
colorscheme moonlight

" User interface
set scrolloff=999 sidescrolloff=5 nowrap
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
map <leader>w <C-w>

tnoremap <esc> <C-\><C-n>
tmap <C-x> <C-\><C-n>
tnoremap <C-h> <C-w>h
tnoremap <C-j> <C-w>j
tnoremap <C-k> <C-w>k
tnoremap <C-l> <C-w>l
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
noremap Y y$
noremap M J
noremap Q K
noremap J <C-d>
noremap K <C-u>
noremap H zH
noremap L zL
map <tab> %
map <S-tab> *
nnoremap <A-j> zo
nnoremap <A-k> zc
map <leader>ot :bottomright split<cr>:terminal<cr>
map <leader>fp :e ~/.config/nvim/init.vim<cr>
map <leader>ff :e 
map <leader>bd :q<cr>
map <leader>qq :qa<cr>
" gc
