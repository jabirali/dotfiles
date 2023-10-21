vim9script

b:ale_fixers = ['isort', 'black']
b:ale_fix_on_save = 1

call ALEMap()
call SlimeMap()

nnoremap <bs> m`O# %%<esc>``
nnoremap <cr> m`/# %%<cr>kVNj"+y<C-w>W<C-c><C-l>%paste -q<cr><C-w>w``
nnoremap <S-cr> <cmd>terminal ipython<cr><C-w>L<C-l><C-w>w
