setlocal nonumber
setlocal wrap

setlocal foldmethod=expr
setlocal foldexpr=vimtex#fold#level(v:lnum)

let b:ale_linters = []
let b:ale_fixers = ['remove_trailing_lines']
let b:ale_fix_on_save = 1

g:vimtex_quickfix_ignore_filters = [
	'Package .* Warning:.*',
	'Underfull \\hbox .*', 
	'Overfull \\hbox.*'
]

