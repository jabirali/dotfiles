setlocal omnifunc=ale#completion#OmniFunc

nnoremap <buffer> K :ALEHover<cr>
nnoremap <buffer> gd :ALEGoToDefinition<cr>
nnoremap <buffer> gD :ALEFindReferences<cr>

let b:ale_fixers = ['isort', 'black']
let b:ale_fix_on_save = 1

vnoremap <cr> :silent! w !~/.config/bin/iterm.sh<cr>

