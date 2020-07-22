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

" Load plugins from the Git submodules in `pack`.
packloadall
silent! helptags ALL

" ANSI color scheme "Vim Dim" with some customization.
set notermguicolors
set background=dark
colorscheme dim

augroup colors
  autocmd!
  " Further dim too strong colors.
  autocmd ColorScheme * highlight Visual ctermbg=black ctermfg=white cterm=NONE
  autocmd ColorScheme * highlight VertSplit ctermbg=NONE ctermfg=black cterm=NONE
  autocmd ColorScheme * highlight StatusLine ctermbg=black cterm=NONE
  autocmd ColorScheme * highlight StatusLineNC ctermbg=black cterm=NONE
  " Override the gutter colors.
  autocmd ColorScheme * highlight SignColumn NONE
  autocmd ColorScheme * highlight NonText ctermfg=8
  autocmd ColorScheme * highlight GitGutterAdd ctermfg=2
  autocmd ColorScheme * highlight GitGutterChange ctermfg=3
  autocmd ColorScheme * highlight GitGutterDelete ctermfg=1
  autocmd ColorScheme * highlight GitGutterChangeDelete ctermfg=1
augroup END
