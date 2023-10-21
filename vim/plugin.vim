vim9script
plug#begin('~/.cache/vim/plug.vim')

Plug 'adigitoleo/vim-mellow', { 'tag': '*' }

Plug 'skywind3000/vim-quickui'
Plug 'tpope/vim-rsi'
#Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-commentary'
Plug 'machakann/vim-sandwich'
Plug 'wellle/targets.vim'
#Plug 'kana/vim-textobj-user'
#Plug 'GCBallesteros/vim-textobj-hydrogen'

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'dense-analysis/ale'
Plug 'jpalardy/vim-slime'
Plug 'tpope/vim-fugitive'
# Plug 'hrsh7th/vim-vsnip'

Plug 'lervag/vimtex'
Plug 'lervag/wiki.vim'
#Plug 'vimwiki/vimwiki'
Plug 'kalekundert/vim-coiled-snake'
Plug 'preservim/vim-markdown'
Plug 'nickeb96/fish.vim'
Plug 'JuliaEditorSupport/julia-vim'

#Plug 'vim/colorschemes'
#Plug 'sainnhe/sonokai'
#Plug 'catppuccin/vim', { 'as': 'catppuccin' }
#Plug 'muellan/am-colors'
#Plug 'itsjunetime/rose-pine-vim'
Plug 'jrudess/vim-foldtext'

# Inbox {{{
# Newly added to my configuration, and thus still under evaluation.
#Plug 'rose-pine/vim'
Plug 'SirVer/ultisnips'
Plug 'Donaldttt/fuzzyy'
Plug 'jremmen/vim-ripgrep'
Plug 'rhysd/clever-f.vim'
Plug 'stefandtw/quickfix-reflector.vim'
Plug 'bronson/vim-visual-star-search'
Plug 'voldikss/vim-floaterm'
#Plug 'ubaldot/vim-outline'
#Plug 'yegappan/lsp'
#Plug 'neoclide/coc.nvim', {'branch': 'release'}
#
# }}}

plug#end()

g:floaterm_title = ''
nnoremap <silent> ` <C-w>:FloatermToggle<cr>
tnoremap <silent> ` <C-w>:FloatermToggle<cr>

nnoremap <S-tab> <C-o>
nnoremap <tab> <C-i>

g:UltiSnipsExpandTrigger = "<tab>"
g:UltiSnipsJumpForwardTrigger = "<tab>"
g:UltiSnipsJumpBackwardTrigger = "<S-tab>"


nnoremap # gcc
vnoremap # gc
