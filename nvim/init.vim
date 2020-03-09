" ~/.config/nvim/init.vim vim: foldmethod=marker foldmarker="\ #,"###

" #1 Neovim settings
" #2 Configuration
set autochdir
set clipboard+=unnamedplus
set completeopt=longest,menuone,noselect
set concealcursor=nc
set conceallevel=2
set confirm
set fillchars=fold:\ ,stl:\ ,stlnc:\ ,vert:\ ,eob:\ ,
set foldlevel=0
set foldmethod=syntax
set gdefault
set hidden
set ignorecase
set inccommand=nosplit
set laststatus=0
set noshowmode
set nowrap
set relativenumber
set scrolloff=999
set shiftwidth=4
set shortmess+=c
set sidescrolloff=5
set signcolumn=no
set smartcase
set spelllang=en,nb
set splitbelow
set splitright
set statusline=%#VertSplit#%=
set tabstop=4
set termguicolors
set tildeop
set undofile
set updatetime=100
set virtualedit=block
set wildmode=longest:full,full
set winaltkeys=no

" #2 Miscellaneous
" Close pop-ups with `q` like Emacs.
augroup quit_like_emacs
	autocmd!
	autocmd BufWinEnter quickfix noremap <buffer> q :q<cr>
	autocmd TermOpen * noremap <buffer> q <c-\><c-n>:close<cr>
	autocmd FileType help noremap <buffer> q :q<cr>
augroup END

" Don't show line numbers in terminals.
augroup terminal_settings
	autocmd!
	autocmd TermOpen * setlocal nonumber norelativenumber scrolloff=0
augroup END

" Define a simple custom folding style. For filetypes that don't have custom
" folding packages loaded, this is much less noisy than the NeoVim defaults.
set foldtext=SimpleFoldText()
function! SimpleFoldText()
	return getline(v:foldstart)
endfunction

" Jump to the Git project root.
command! GitCd execute 'cd ./'.system('git rev-parse --show-cdup')

" Search through Zotero library.
command! -bang Zotero call fzf#run(fzf#wrap(
			\ 'zotero',
			\ { 'source':  'fdfind -t f -e pdf . ~/.zotero/',
			\   'sink':    'silent !zathura --fork',
			\   'options': '-m -d / --with-nth=-1' },
			\ <bang>0))

" #1 Plugin settings
" #2 Configuration
" Plugin parameters.
let g:ale_fixers = {'*': ['trim_whitespace'], 'python': ['black', 'isort']}
let g:ale_fix_on_save = 1
let g:ale_linters = {'python': ['flake8'], 'tex': []}
let g:ale_set_quickfix = 1
let g:clever_f_chars_match_any_signs = ' '
let g:clever_f_smart_case = 1
let g:coiled_snake_foldtext_flags = []
let g:fold_cycle_default_mapping = 0
let g:fzf_action = { 'ctrl-s': 'split', 'ctrl-v': 'vsplit' }
let g:fzf_layout = { 'window': 'enew' }
let g:fzf_colors =
  \ { 'fg':      ['fg', 'Normal'],
    \ 'bg':      ['bg', 'Normal'],
    \ 'hl':      ['fg', 'Comment'],
    \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
    \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
    \ 'hl+':     ['fg', 'Statement'],
    \ 'info':    ['fg', 'PreProc'],
    \ 'border':  ['fg', 'Ignore'],
    \ 'prompt':  ['fg', 'Conditional'],
    \ 'pointer': ['fg', 'Exception'],
    \ 'marker':  ['fg', 'Keyword'],
    \ 'spinner': ['fg', 'Label'],
    \ 'header':  ['fg', 'Comment'] }
let g:fzf_history_dir = '~/.local/share/fzf'
let g:loaded_netrw = 1
let g:magit_default_fold_level = 1
let g:nnn#replace_netrw = 1
let g:nuake_per_tab = 1
let g:nuake_position = 'top'
let g:nv_search_paths = ['~/projects/notes']
let g:org_aggressive_conceal = 1
let g:pandoc#folding#fdc = 0
let g:semshi#mark_selected_nodes = 0
let g:tex_conceal = 'abdgm'
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'
let g:vimade = {'fadelevel': 0.7}
let g:vimtex_compiler_progname = 'nvr'
let g:vimtex_fold_enabled = 1
let g:vimtex_view_method = 'zathura'
let loaded_netrwPlugin = 1

" Syntax highlights.
augroup clean_highlights
	autocmd!
	" Suddle highlighting of active window.
	autocmd ColorScheme * hi! InactiveWindow guibg=none
	autocmd ColorScheme * hi! ActiveWindow guibg='#23253a'
	set winhighlight=Normal:ActiveWindow,NormalNC:InactiveWindow
	" Tone down too heavy default highlighting.
	autocmd ColorScheme * hi! LineNr guibg=none
	autocmd ColorScheme * hi! link CursorLineNr LineNr
	autocmd ColorScheme * hi! Conceal guifg=none guibg=none
	autocmd ColorScheme * hi! MatchParen guifg=none gui=bold
	autocmd ColorScheme * hi! CleverFDefaultLabel guifg=white gui=bold
	autocmd ColorScheme * hi! SpellBad guifg=red gui=none
augroup END

" #2 Installation
" Bootstrap procedure.
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
	silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * nested PlugInstall --sync | source $MYVIMRC
endif

" Load the plugins.
call plug#begin('~/.local/share/nvim/plugins')
	" User interface
	Plug 'Brettm12345/moonlight.vim'    " Moonlight colorscheme
 	Plug 'arecarn/vim-fold-cycle'       " Org-like recursive folding
	Plug 'Konfekt/FastFold'             " More optimized code folding
	Plug 'liuchengxu/vim-which-key'     " Interactive keybinding help
	Plug 'tpope/vim-repeat'             " Repeat more things with `.`
	Plug 'tpope/vim-unimpaired'         " Browse more things with `[]`
	Plug 'junegunn/goyo.vim'            " Center buffer when writing
	" Text editing
	Plug 'andymass/vim-matchup'         " Smarter `%` jumps and highlights
	Plug 'junegunn/vim-slash'           " Smarter `*` jumps and highlights
	Plug 'rhysd/clever-f.vim'           " Smarter `f` jumps and highlights
	Plug 'machakann/vim-sandwich'       " More intuitive `surround` plugin
	Plug 'tpope/vim-commentary'         " Faster (un)commenting with `gc`
	Plug 'junegunn/vim-easy-align'      " Faster code alignment with `ga`
	Plug 'tpope/vim-speeddating'        " Better c-a/c-x actions for dates
	" Searching
	Plug 'junegunn/fzf',                {'do': './install --bin'}
	Plug 'junegunn/fzf.vim'             " Fuzzy finding of everything
	Plug 'alok/notational-fzf-vim'      " Fuzzy finding of notes
	" Version control
	Plug 'tpope/vim-fugitive'           " Version control (general)
	Plug 'jreybert/vimagit'             " Version control (commit)
	Plug 'airblade/vim-gitgutter'       " Version control (buffer)
	" IDE features
	Plug 'neovim/nvim-lsp'              " Built-in LSP (configs)
	Plug 'dense-analysis/ale'           " Linters and formatters
	Plug 'sirver/ultisnips'             " Snippets (the engine)
	Plug 'honza/vim-snippets'           " Snippets (collection)
	" Plug 'lifepillar/vim-mucomplete'    " Minimalist autocompletion
	Plug 'Lenovsky/nuake'               " Per-tab pop-up terminal
	Plug 'mcchrish/nnn.vim'             " File browser
	" Python
	Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins'}
	Plug 'jmcantrell/vim-virtualenv'
	Plug 'tmhedberg/SimpylFold'
	" LaTeX
	Plug 'lervag/vimtex'
	Plug 'PietroPate/vim-tex-conceal'
	" Markup
	Plug 'vim-pandoc/vim-pandoc-syntax'
	Plug 'vim-pandoc/vim-pandoc'
	Plug 'jceb/vim-orgmode'
	" Config
	Plug 'cespare/vim-toml'
call plug#end()

" Activate themes.
set background=dark
set guifont=Iosevka\ SS09\ Light:h16
silent! colorscheme moonlight

" Activate LSP.
lua require'nvim_lsp'.bashls.setup{}
lua require'nvim_lsp'.pyls.setup{}
lua require'nvim_lsp'.texlab.setup{}
lua require'nvim_lsp'.vimls.setup{}
autocmd Filetype sh,python,tex,vim setlocal omnifunc=v:lua.vim.lsp.omnifunc


" #1 Keybindings
" #2 Leader keys
" Spacemacs-like leaders.
let mapleader="\<space>"
let maplocalleader=","

" Spacemacs-like hints.
nnoremap <silent> <leader>      :WhichKey '<space>'<cr>
nnoremap <silent> <localleader> :WhichKey ','<cr>

" Window splits.
map <leader>w <c-w>
map <leader>wd :close<cr>

" Tabs/Workspaces.
map <leader><tab>1 :1tabnext<cr>
map <leader><tab>2 :2tabnext<cr>
map <leader><tab>3 :3tabnext<cr>
map <leader><tab>4 :4tabnext<cr>
map <leader><tab>5 :5tabnext<cr>
map <leader><tab>6 :6tabnext<cr>
map <leader><tab>7 :7tabnext<cr>
map <leader><tab>8 :8tabnext<cr>
map <leader><tab>9 :9tabnext<cr>
map <leader><tab>d :tabclose<cr>
map <leader><tab>n :tabnew<cr>
map <leader><tab>] :tabnext<cr>
map <leader><tab>[ :tabprev<cr>
map ]<tab> :tabnext<cr>
map [<tab> :tabprev<cr>

" Toggle stuff.
map <leader>t yo
map <leader>tg :Goyo<cr>
map <leader>tv :VirtualEnvActivate<space>
map <leader>tV :VirtualEnvDeactivate<cr>

" Open stuff.
map <leader>od :NnnPicker '%:p:h'<cr>
map <leader>ol :bot lwindow<cr>
map <leader>oo :tabedit ~/projects/notes/sintef.org<cr>
map <leader>oO :tabedit ~/projects/notes/personal.org<cr>
map <leader>on :NV<cr>
map <leader>op :bot split term://htop<cr>i
map <leader>ot :Nuake<cr><c-\><c-n>:set scrolloff=999<cr>:<c-c>
map <leader>oT :terminal<cr>
map <leader>ov <leader>tv
map <leader>oV <leader>tV
map <leader>oq :bot cwindow<cr>

" Find files.
map <leader>ff :Files ~/projects/<cr>
map <leader>fo :Files ~/onedrive/<cr>
map <leader>fF :Files ~/<cr>
map <leader>fb :Buffers<cr>
map <leader>fd :Files ~/.config/<cr>
map <leader>fD :Files /etc/<cr>
map <leader>fg :GFiles<cr>
map <leader>fG :GFiles?<cr>
map <leader>fn :Files ~/projects/notes/<cr>
map <leader>fp :Files ~/projects/<cr>
map <leader>fr :History<cr>
map <leader>fz :Zotero!<cr>

" Version control.
map <leader>gg :MagitOnly<cr>:set scrolloff=999<cr>
map <leader>gb :GBlame<cr>
map <leader>gc :BCommits<cr>
map <leader>gC :Commits<cr>
map <leader>gd :Git difftool<cr>
map <leader>gD :Gdelete<space>
map <leader>gf :Gfetch<cr>
map <leader>gl :Glog<cr>
map <leader>gm :Git mergetool<cr>
map <leader>gp :Gpush<cr>
map <leader>gR :Gmove<space>
map <leader>gs <Plug>(GitGutterStageHunk)
map <leader>gu <Plug>(GitGutterUndoHunk)
map <leader>gz :GitGutterSignsToggle<cr>:GitGutterFold<cr>

" Search content.
map <leader>ss :BLines<cr>
map <leader>s* :Ggrep <c-R><c-W><cr>
map <leader>sg :Ggrep<space>
map <leader>sn :cd ~/projects/notes/<cr>:Rg<cr>
map <leader>sp :GitCd<cr>:Rg<cr>
map <leader>s] :BTags<cr>

" Manage Vim.
map <leader>qa :qa<cr>
map <leader>qr :source ~/.config/nvim/init.vim<cr>
map <leader>qi :PlugInstall<cr>
map <leader>qu :PlugUpdate<cr>

" Leader-based speed keys.
map <leader><leader> <leader>fg
map <leader>, <leader>fb
map <leader>. <leader>od
map <leader>/ <leader>s]
map <leader>: :Commands<cr>
map <leader>h :Helptags<cr>

" #2 Editor keys
" Better defaults.
nnoremap ' `
nnoremap Y y$
noremap  j gj
noremap  k gk
nnoremap U <c-r>
noremap  R :silent bunload<cr>:silent buffer #<cr>

" Paging with HJKL.
noremap H zH
noremap L zL
noremap J <c-d>
noremap K <c-u>

" Org-like indentation.
nmap <M-h> <<
nmap <M-l> >>
vmap <M-h> <gv
vmap <M-l> >gv

" Org-like dragging.
nmap <M-j> ]e
nmap <M-k> [e
vmap <M-j> ]egv
vmap <M-k> [egv

" Look around.
" noremap <M-h> zH
" noremap <M-j> <C-d>
" noremap <M-k> <C-u>
" noremap <M-l> zL

" Ergononmic matching jumps.
" map <bs> %
" map <cr> *

" Ergonomic code folding.
nmap <tab> <Plug>(fold-cycle-open)
nnoremap <s-tab> zm

" Align expressions.
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)

" Relocated keys.
noremap Q K
noremap M J

" Escape to normal mode with `jk`.
inoremap jk <esc>

" Escape to command mode with `;`.
nnoremap ; :

" Escape to normal mode in terminals.
augroup terminal_escape
	autocmd!
	autocmd TermOpen * tnoremap <buffer> <esc> <c-\><c-n>
	autocmd FileType fzf tunmap <buffer> <esc>
augroup END

" FZF bindings.
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-l> <plug>(fzf-complete-line)

" TeX bindings.
nnoremap <localleader>lt :call vimtex#fzf#run()<cr>

" LSP bindings.
nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<cr>
nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<cr>
nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<cr>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<cr>
nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<cr>
nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<cr>
nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<cr>
nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<cr>
