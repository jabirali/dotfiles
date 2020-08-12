" ~/.config/nvim/init.vim

" Barebones Neovim configuration. This configuration uses the built-in `packadd`,
" and all plugins are Git submodules in my dotfiles repository. Since I'm trying
" to migrate to an IDE for longer programming sessions, and retaining Neovim only
" for quick edits in the terminal, this config is more barebones than my old ones.

" Better defaults.
set autochdir
set foldmethod=syntax
set spelllang=en,nb
set tildeop

" Interface settings.
set fillchars=fold:\ ,
set nowrap
set signcolumn=yes:1
set splitbelow
set splitright
set updatetime=100
set winaltkeys=no

" Search settings.
set gdefault
set ignorecase
set inccommand=nosplit
set smartcase

" Indent settings.
set expandtab
set shiftwidth=2
set softtabstop=2
set tabstop=2

" Plugin settings.
let g:fzf_layout = { 'window': { 'width': 0.80, 'height': 0.65, 'xoffset': 0.50, 'yoffset': 0.45 } }

" Load plugins from the Git submodules in `pack`.
packloadall
silent! helptags ALL

" ANSI color scheme "Vim Dim" with some customization.
colorscheme minimono

" Keybindings.
nmap <C-p> :GFiles<cr>
