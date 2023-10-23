let b:ale_fixers = ['isort', 'black']
let b:ale_fix_on_save = 1

call ALEMap()

nnoremap <buffer> <cr> <cmd>silent w<bar>only<bar>terminal ipython -i %<cr>
