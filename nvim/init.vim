" ~/.config/nvim/init.vim

" Barebones Neovim configuration. This configuration uses the built-in `packadd`,
" and all plugins are Git submodules in my dotfiles repository. Since I'm trying
" migrate to VSCode more advanced refactoring and debugging, and use Neovim mainly
" for more basic day-to-day programming, my new config relatively vanilla.

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
set hidden
set modifiable
set relativenumber
set scrolloff=999
set signcolumn=yes
set virtualedit=block

" Custom colorscheme.
colorscheme minimono

" Plugin settings.
let g:SimpylFold_fold_import = 0
let g:tex_flavor = 'latex'

" Zettelkasten setup.
let g:wiki_root = '~/Documents/Wiki'
let g:wiki_filetypes = ['md']
let g:wiki_link_target_type = 'md'

let g:wiki_map_link_create = 'ZettelLink'
function ZettelLink(text) abort
  return printf("0x%08x", str2nr(strftime('%s')))
endfunction
