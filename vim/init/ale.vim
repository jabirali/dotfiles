let g:ale_virtualtext_cursor = 0
let g:ale_floating_preview = 1
let g:ale_floating_window_border = ['│', '─', '┌', '┐', '┘', '└', '│', '─']

function g:ALEMap()
	setlocal omnifunc=ale#completion#OmniFunc
	nnoremap <buffer> K :ALEHover<cr>
	nnoremap <buffer> gd :ALEGoToDefinition<cr>
	nnoremap <buffer> gD :ALEFindReferences<cr>
endfunction
