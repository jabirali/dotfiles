"------------------------------------------------------------
" File: Neovim config
" Path: ~/.config/nvim/init.vim
"------------------------------------------------------------

" |=====================================|
" |          PLUGIN MANAGEMENT          |
" |=====================================|

" Plugin manager
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

" Plugin install
call plug#begin('~/.local/share/nvim/plugins')
  Plug 'vim-ctrlspace/vim-ctrlspace'
  Plug 'airblade/vim-gitgutter'
  Plug 'morhetz/gruvbox'
  Plug 'lervag/vimtex'
  Plug 'dbmrq/vim-ditto'
  Plug 'rhysd/vim-grammarous'
  Plug 'tweekmonster/braceless.vim'
  Plug 'godlygeek/tabular'
  Plug 'plasticboy/vim-markdown'
call plug#end()

" Plugin settings
let g:CtrlSpaceDefaultMappingKey = "<leader><leader> "
let g:ctrlp_working_path_mode           = 'r'
let g:gitgutter_sign_added              = '+'
let g:gitgutter_sign_modified           = '~'
let g:gitgutter_sign_removed            = '-'
let g:gitgutter_sign_removed_first_line = '^'
let g:gitgutter_sign_modified_removed   = '~'



" |======================================|
" |            BASIC SETTINGS            |
" |======================================|

" Color scheme
let g:gruvbox_contrast_light="hard"
let g:gruvbox_contrast_dark="hard"
set background=dark
colorscheme gruvbox
highlight Normal  ctermbg=None
highlight NonText ctermbg=None ctermfg=Gray guifg=Gray

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
set tildeop
set autochdir

" Tabs and trailing spaces
set list
set listchars=tab:│\ ,nbsp:~,extends:›,precedes:‹
hi NonText ctermfg=darkgrey
hi SpecialKey ctermfg=darkgrey



" |=====================================|
" |             KEYBINDINGS             |
" |=====================================|

" Leader definition
let mapleader="\<space>"

" Leader shortcuts
nnoremap <leader><leader> <C-w><C-w>

" Common commands
vnoremap s :s//gc<left><left><left>
nnoremap s :%s//gc<left><left><left>
vnoremap \| :!
nnoremap \| :%!

" Redraw the screen
nnoremap <silent> <C-l>   :nohl<cr><C-l>

" Tab management
nnoremap <C-t>            :tabnew<cr>
nnoremap {                :tabprev<cr>
nnoremap }                :tabnext<cr>

" Folding
nnoremap  <tab>           zr
nnoremap  <S-tab>         zm
     
" Misc remapped bindings
noremap   Y               y$
nnoremap  Q               @q

" Greek symbols
inoremap  ;              *
inoremap  ;s             σ
inoremap  ;P             Π
inoremap  ;j             ∂

" |=====================================|
" |             FILE TYPES              |
" |=====================================|

" Python settings
au FileType python setlocal tabstop=4 shiftwidth=4 expandtab! foldenable foldlevelstart=0 foldnestmax=3

" Text files
au BufNewFile,BufRead *.txt,*.md,*.markdown set filetype=markdown textwidth=79
au FileType markdown,text,tex DittoOn
au FileType markdown,text,tex set spell

" Filetype: Fortran
au BufNewFile,BufRead *.f,*.i set filetype=fortran
au FileType fortran nnoremap <buffer> K :vsplit<bar>wincmd w<bar>e term://pinfo\ gfortran\ --node\ \"Keyword\ Index\"<cr>i
au FileType fortran syn keyword Type impure
au FileType fortran syn keyword PreProc block
au FileType fortran syn keyword PreProc submodule
let fortran_free_source=1
let fortran_do_enddo=1

" Filetype: Latex
let g:vimtex_fold_enabled=1
"let g:latex_view_general_viewer = 'evince'
"let g:vimtex_view_method        = 'evince'
"let g:vimtex_compiler_progname  = 'latexmk'
