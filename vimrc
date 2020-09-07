" ~/.vimrc

" Neovim defaults.
filetype plugin indent on
set autoindent
set autoread
set background=dark
set backspace=indent,eol,start
set backupdir=~/.local/share/nvim/backup
set belloff=all
set cscopeverbose
set directory=~/.local/share/nvim/swap/
set encoding=UTF-8
set fillchars=vert:│,fold:·
set formatoptions=tcqj
set history=10000
set hlsearch
set incsearch
set langnoremap
set laststatus=2
set nofsync
set nolangremap
set nrformats=bin,hex
set ruler
set sessionoptions-=options
set shortmess+=F
set shortmess-=S
set showcmd
set sidescroll=1
set smarttab
set tabpagemax=50
set tags=./tags;,tags
set ttimeoutlen=50
set ttyfast
set undodir=~/.local/share/nvim/undo
set viminfo+=!
set wildmenu

" Neovim config.
source ~/.config/nvim/init.vim
