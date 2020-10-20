" ~/.gvimrc

" Aesthetic settings.
set guifont=Monaco:h12
set guioptions=egm
set termguicolors
let ayucolors="mirage"
colorscheme ayu

" MacOS standard bindings.
nnoremap <space>   <C-d>
nnoremap <S-space> <C-u>

" iTerm2-like bindings.
nnoremap <D-]> <C-w>w
nnoremap <D-[> <C-w>W
nnoremap <D-d> :vsplit<cr>
nnoremap <D-D> :split<cr>

" Sublime-like bindings.
nnoremap <D-F> :CtrlSF 
nnoremap <D-O> :GFiles<cr>

" Custom keybindings.
nnoremap <D-K> :Dash<cr>
