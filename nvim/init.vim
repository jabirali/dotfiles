" ~/.config/nvim/init.vim

" Barebones Neovim configuration. This configuration uses the built-in `packadd`,
" and all plugins are Git submodules in my dotfiles repository. Since I'm trying
" to migrate to an IDE for longer programming sessions, and retaining Neovim only
" for quick edits in the terminal, this config is more barebones than my old ones.

" Built-in settings.
set autochdir
set expandtab
set fillchars=fold:\ ,
set foldmethod=syntax
set gdefault
set ignorecase
set inccommand=nosplit
set nowrap
set signcolumn=yes:1
set smartcase
set spelllang=en,nb
set splitbelow
set splitright
set tildeop
set updatetime=100
set winaltkeys=no

" Plugin settings.
let g:fzf_layout = { 'window': { 'width': 0.80, 'height': 0.65, 'xoffset': 0.50, 'yoffset': 0.45 } }

" Load plugins from the Git submodules in `pack`.
packloadall
silent! helptags ALL

" ANSI color scheme "Vim Dim" with some customization.
colorscheme minimono

" Custom keybindings. These bindings are not only used in Neovim, but also
" loaded automatically by the `VSCodeVim` extension in Visual Studio Codium.
" By rebinding control characters to regular symbols, I can use the control
" keyboard shortcuts of IDE's like VSCode without having to remap everything.
nnoremap U <C-r>
nnoremap M J
nnoremap K <C-v>k
nnoremap J <C-v>j
vnoremap K k
vnoremap J j
