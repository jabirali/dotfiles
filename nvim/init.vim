" ~/.config/nvim/init.vim

" Sane defaults.
set autochdir
set clipboard=unnamed
set fillchars=fold:\ ,
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

" Aesthetics
let g:nord_bold = 1
let g:nord_italic = 1
let g:nord_uniform_diff_background = 1
colorscheme nord

let g:lightline = {}
let g:lightline.colorscheme = 'nord'
let g:lightline.separator = { 'left': "\ue0b0", 'right': "\ue0b2" }
let g:lightline.subseparator = { 'left': "\ue0b1", 'right': "\ue0b3" }

" Miscellaneous
helptags ALL
