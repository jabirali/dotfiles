" ~/.gvimrc

" Aesthetic settings.
set guifont=Monaco:h12
set guioptions=egm
set lines=999
set columns=999
set scrolloff=999
set number
set signcolumn=yes
let ayucolors="mirage"
colorscheme ayu

" iTerm2-like bindings.
nnoremap <D-]> <C-w>w
nnoremap <D-[> <C-w>W
nnoremap <D-d> :vsplit<cr>
nnoremap <D-D> :split<cr>

nnoremap <D-F> :CtrlSF 
