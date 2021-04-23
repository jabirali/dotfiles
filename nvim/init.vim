" ~/.config/nvim/init.vim

" Sane defaults.
set autochdir
set clipboard=unnamed
set fillchars=fold:\ ,
set foldminlines=0
set foldmethod=indent
set ignorecase
set inccommand=nosplit
set noshowmode
set nowrap
set relativenumber
set scrolloff=999
set shiftwidth=4
set signcolumn=yes
set signcolumn=yes
set smartcase
set softtabstop=4
set spelllang=en,nb
set splitbelow
set splitright
set tabstop=4
set tildeop
set updatetime=100
set virtualedit=block
set winaltkeys=no

" Nice aesthetics.
let g:nordbold = 1
let g:nord_italic = 1
let g:nord_uniform_diff_background = 1
colorscheme nord

let g:lightline = {}
let g:lightline.colorscheme = 'nord'
let g:lightline.separator = { 'left': "\ue0b0", 'right': "\ue0b2" }
let g:lightline.subseparator = { 'left': "\ue0b1", 'right': "\ue0b3" }

augroup Highlights
  autocmd!
  autocmd ColorScheme * hi Folded ctermbg=none guibg=none 
augroup END

" Tilish integration.
noremap <silent> <m-h> :TmuxNavigateLeft<cr>
noremap <silent> <m-j> :TmuxNavigateDown<cr>
noremap <silent> <m-k> :TmuxNavigateUp<cr>
noremap <silent> <m-l> :TmuxNavigateRight<cr>

" Miscellaneous.
helptags ALL
