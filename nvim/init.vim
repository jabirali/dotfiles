" ~/.config/nvim/init.vim

" Barebones Neovim configuration. This configuration uses the built-in `packadd`,
" and all plugins are Git submodules in my dotfiles repository. Since I'm trying
" migrate to Doom Emacs for advanced programming and debugging, and use Neovim
" mainly for quick day-to-day scripts and config, my new config is simple.

" Generate tags.
helptags ALL

" Better defaults.
set autochdir
set foldmethod=syntax
set spelllang=en,nb
set tildeop

" Interface settings.
set fillchars=fold:\ ,
set nowrap
set signcolumn=yes
set splitbelow
set splitright
set updatetime=100
set winaltkeys=no

" Search settings.
set gdefault
set ignorecase
set smartcase

" Indent settings.
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4

" Miscellaneous.
set relativenumber
set scrolloff=999
set signcolumn=yes
set virtualedit=block

" Custom colorscheme.
set termguicolors
packadd! onedark.vim
colorscheme onedark
