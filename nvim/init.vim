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

" Plugin settings.
" let g:fzf_layout = { 'window': { 'width': 0.80, 'height': 0.65, 'xoffset': 0.50, 'yoffset': 0.45 } }

" ANSI color scheme "Vim Dim" with some customization.
"colorscheme minimono

" Keybindings.
nmap <C-p> :GFiles<cr>
