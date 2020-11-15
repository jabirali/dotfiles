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
let g:vim_markdown_folding_disabled = 1
let g:wiki_root = '~/Documents/Wiki'
let g:wiki_filetypes = ['md']
let g:wiki_link_target_type = 'md'

let g:wiki_map_link_create = 'ZettelLink'
function ZettelLink(text) abort
  return printf("0x%08x.md", str2nr(strftime('%s')))
endfunction

" Custom keybindings
nnoremap Q mqgqip`q
nnoremap <silent> <leader>wi <c-w>p:let @" = expand("%")<cr><c-w>pi[]()<esc>Pl%hi

function! RipgrepFzf(query, fullscreen)
  let command_fmt = "echo %s | tr -s ' ' '\n' | rg --color=always --count --files-with-matches --smart-case -f -" 
  "| sed 's/\(.*\):\([0-9]*\)/\2:\1/' | sort -gr | sed 's/[0-9]*:\(.*\)/\1/'"
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  " call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
  call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
endfunction

command! -nargs=* -bang RG call RipgrepFzf(<q-args>, <bang>0)
