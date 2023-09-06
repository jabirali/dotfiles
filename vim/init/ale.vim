vim9script

g:ale_virtualtext_cursor = 0
g:ale_floating_preview = 1
g:ale_floating_window_border = [] #['│', '─', '╭', '╮', '╯', '╰', '│', '─']

def g:ALEMap()
	setlocal omnifunc=ale#completion#OmniFunc

	nnoremap <buffer> K :ALEHover<cr>
	nnoremap <buffer> gd :ALEGoToDefinition<cr>
	nnoremap <buffer> gD :ALEFindReferences<cr>
enddef
