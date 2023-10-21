vim9script

setlocal nonumber
setlocal wrap

setlocal foldmethod=expr
setlocal foldexpr=vimtex#fold#level(v:lnum)

b:ale_linters = []
#b:ale_fixers = ['latexindent', 'remove_trailing_lines']
b:ale_fixers = ['remove_trailing_lines']
b:ale_fix_on_save = 1

g:vimtex_quickfix_ignore_filters = [
	'Package .* Warning:.*',
   	'Underfull \\hbox .*', 
	'Overfull \\hbox.*'
]

