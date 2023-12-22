setlocal nonumber
setlocal wrap
setlocal breakindentopt+=list:-1

nnoremap <buffer> <localleader>p :Pandoc! pdf<cr>
nnoremap <buffer> <localleader>w :Pandoc! docx<cr>
