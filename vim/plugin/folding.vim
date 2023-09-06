" set foldtext=FoldText()
" function FoldText()
"     let indent = indent(v:foldstart) - &sw
" 	return repeat('    ', indent) . trim(getline(v:foldstart))
" endfunction

" nnoremap <tab> za
" nnoremap <S-tab> zM
