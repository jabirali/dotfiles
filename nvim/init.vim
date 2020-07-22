" ~/.config/nvim/init.vim

" I'm in the process of migrating from `vim-plug` to `packadd`, since the
" latter is built into `nvim`, and also works well with Git submodules. Both
" of these enhance portability: you can have a standard `nvim` on a different
" computer, do a `git clone --recursive`, and get all your plugins with you.
" Since my `init.vim` has accumulated a lot of keybindings and plugins that
" I don't actively use, this is also a nice opportunity to clean up a bit:
" I've disabled everything, and will slowly turn on again the stuff I use.

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

let g:gitgutter_sign_added = '+'
let g:gitgutter_sign_modified = '~'
let g:gitgutter_sign_modified_removed = '~'
let g:gitgutter_sign_removed = '▁'
let g:gitgutter_sign_removed_first_line = '▔'

" Load plugins from the Git submodules in `pack`.
packloadall
silent! helptags ALL

" ANSI color scheme "Vim Dim" with some customization.
set notermguicolors
set background=dark
colorscheme dim

augroup colors
  autocmd!
  autocmd ColorScheme * highlight Visual ctermbg=black ctermfg=white cterm=NONE
  autocmd ColorScheme * highlight SignColumn ctermbg=NONE
  autocmd ColorScheme * highlight StatusLine ctermbg=black cterm=NONE
  autocmd ColorScheme * highlight StatusLineNC ctermbg=black cterm=NONE
augroup END
