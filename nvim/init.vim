" ~/.config/nvim/init.vim

" Barebones Neovim configuration. This configuration uses the built-in `packadd`,
" and all plugins are Git submodules in my dotfiles repository. Since I'm trying
" migrate to VSCode more advanced refactoring and debugging, and use Neovim mainly
" for more basic day-to-day programming, my new config relatively vanilla.

" Better defaults.
set autochdir
set foldmethod=syntax
set spelllang=en,nb
set tildeop
set termguicolors

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
set lines=999
set columns=999
set scrolloff=999
set relativenumber
set signcolumn=yes

" Custom colorscheme.
colorscheme minimono
