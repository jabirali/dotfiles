" vim:foldmethod=marker

" -----------------------------------------------------------
" File: Neovim config
" Path: ~/.config/nvim/init.vim
" -----------------------------------------------------------

" Plugins {{{ 

" Install the plugin manager
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Plugin install
call plug#begin('~/.local/share/nvim/plugins')
  " User interface
  Plug 'Brettm12345/moonlight.vim' " Moonlight colorscheme
  Plug 'Konfekt/FastFold'          " More optimized code folding
  Plug 'liuchengxu/vim-which-key'  " Interactive keybinding help
  " Text editing
  Plug 'andymass/vim-matchup'      " Smarter `%` jumps and highlights
  Plug 'junegunn/vim-slash'        " Smarter `*` jumps and highlights
  Plug 'tpope/vim-commentary'      " Smarter (un)commenting with `gc`
  " Language support
  Plug 'lervag/vimtex'             " *.tex
  Plug 'jceb/vim-orgmode'          " *.org
  Plug 'plasticboy/vim-markdown'   " *.md
  "Plug 'vim-ctrlspace/vim-ctrlspace'
  Plug 'junegunn/fzf'
  Plug 'junegunn/fzf.vim'
call plug#end()
" }}}

augroup matchup_matchparen_highlight
  autocmd!
  autocmd ColorScheme * hi MatchParen guifg=red gui=none
augroup END


au TermOpen * setlocal listchars= nonumber norelativenumber

" Color scheme
set termguicolors
set background=dark
colorscheme moonlight

" User interface
set scrolloff=999 
set sidescrolloff=5
set nowrap
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

" Spacemacs-like leaders.
let mapleader="\<space>"
let maplocalleader=","

" Provide interactive hints.
nnoremap <silent> <leader>      :WhichKey   '<space>'<cr>
nnoremap <silent> <localleader> :WhichKey   ','<cr>
" nnoremap <silent>               [ :WhichKey '['<cr>
" nnoremap <silent>               ] :WhichKey ']'<cr>
" nnoremap <silent>               g :WhichKey 'g'<cr>
" nnoremap <silent>               z :WhichKey 'z'<cr>

" Spacemacs-like menu.
map <leader>w <C-w>
map <leader>ot :bot split term://fish<cr>i
map <leader>of :bot split term://nnn<cr>i
map <leader>op :bot split term://htop<cr>i
map <leader>fp :e ~/.config/nvim/init.vim<cr>
map <leader>ff :e 
map <leader>bd :q<cr>
map <leader>qq :qa<cr>
map <leader>qr :source ~/.config/nvim/init.vim<cr>
map <leader>qu :PlugUpdate<cr>

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
"let g:CtrlSpaceDefaultMappingKey = "<leader><leader> "
" gc
