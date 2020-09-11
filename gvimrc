" ~/.gvimrc

" Aesthetic settings.
set guifont=Monaco:h12
set guioptions=egm
set termguicolors
let ayucolors="mirage"
colorscheme ayu

" iTerm2-like bindings.
nnoremap <D-]> <C-w>w
nnoremap <D-[> <C-w>W
nnoremap <D-d> :vsplit<cr>
nnoremap <D-D> :split<cr>

" Sublime-like bindings.
nnoremap <D-F> :CtrlSF 
nnoremap <D-P> :GFiles<cr>
