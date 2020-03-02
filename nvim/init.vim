" ~/.config/nvim/init.vim vim: foldmethod=marker foldmarker="\ #,"###

" #1 Neovim settings
" #2 Configuration
set autochdir
set clipboard+=unnamedplus
set complete=t,i,d,.
set completeopt=longest,menu,preview
set concealcursor=nc
set conceallevel=2
set confirm
set cursorline
set fillchars=fold:\ ,stl:-,stlnc:-,
set foldlevel=0
set foldmethod=syntax
set gdefault
set hidden
set ignorecase
set inccommand=nosplit
set laststatus=0
set noshowmode
set nowrap
set nonumber
set relativenumber
set scrolloff=999
set shiftwidth=4
set sidescrolloff=5
set smartcase
set spelllang=en,nb
set splitbelow
set splitright
set statusline=%#VertSplit#%=
set tabstop=4
set termguicolors
set tildeop
set updatetime=100
set virtualedit=block
set wildmode=longest:full,full
set winaltkeys=no

" #2 Miscellaneous
" Close pop-ups with `q` like Emacs.
augroup quit_like_emacs
	autocmd!
	autocmd BufWinEnter quickfix noremap <buffer> q :q<cr>
	autocmd FileType help noremap <buffer> q :q<cr>
augroup END

" Don't show line numbers or cursorlines in terminals.
augroup terminal_settings
	autocmd!
	autocmd TermOpen * setlocal nonumber norelativenumber nocursorline
augroup END

" Don't show line numbers etc. in browsers.
if exists('g:started_by_firenvim')
	set nonumber signcolumn=no statusline=%=
endif

" Define a simple custom folding style. It basically just shows the first line
" of the folded text. If the end of the folding pattern contains an opening
" marker, it is replaced by or accompandied by an appropriate closing marker.
function! SimpleFoldText()
	let line1 = getline(v:foldstart)

	" Replace `def f(x):` with `def f(x);`.
	let line2 = substitute(line1, ':\s*$', ';', '')
	" Replace `int f(int x) {` with `int f(int x) {...}`.
	let line3 = substitute(line2, '[\{][\{]*\s*$', '{...}', '')
	" Replace `"""First line.` with `"""First line."""`
	let line4 = substitute(line3, '"""\(.*\)', '"""\1"""', '')

	return line4
endfunction

" Use this folding style everywhere.
set foldtext=SimpleFoldText()
augroup fold_with_style
	autocmd!
	autocmd VimEnter,WinEnter,BufWinEnter * setlocal foldtext=SimpleFoldText()
augroup END

" Jump to the Git project root.
command! GitCd execute 'cd ./'.system('git rev-parse --show-cdup')

" #1 Plugin settings
" #2 Configuration
" Plugin parameters.
let b:ale_fixers = { '*': [ 'trim_whitespace' ], 'python': [ 'black', 'isort' ] }
let g:ale_fix_on_save = 1
let g:ale_linters = { 'python': [ 'flake8' ], 'tex': [] }
let g:clever_f_chars_match_any_signs = '.'
let g:clever_f_smart_case = 1
let g:fold_cycle_default_mapping = 0
let g:loaded_netrw = 1
let g:magit_default_fold_level = 1
let g:gitgutter_signs = 0
let g:nnn#replace_netrw = 1
let g:nuake_per_tab = 1
let g:nuake_position = 'top'
let g:org_aggressive_conceal = 1
let g:SimpylFold_docstring_preview = 1
let g:sexp_filetypes = ''
let g:tex_conceal = 'abdgm'
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<C-k>'
let g:UltiSnipsJumpForwardTrigger = '<C-j>'
let g:UltiSnipsListSnippets = '<S-tab>'
let g:ultisnips_python_style = 'google'
let g:vim_markdown_folding_style_pythonic = 1
let g:vim_markdown_override_foldtext = 0
let g:vimtex_compiler_progname = 'nvr'
let g:vimtex_fold_enabled = 1
let g:vimtex_view_method = 'zathura'
let loaded_netrwPlugin = 1

" Syntax highlights.
augroup clean_highlights
	autocmd!
	autocmd ColorScheme * hi MatchParen guifg=none gui=bold
	autocmd ColorScheme * hi CleverFDefaultLabel guifg=white gui=bold
	autocmd ColorScheme * hi SpellBad guifg=red gui=none
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
	Plug 'Brettm12345/moonlight.vim'  " Moonlight colorscheme
 	Plug 'arecarn/vim-fold-cycle'     " Org-like recursive folding
	Plug 'Konfekt/FastFold'           " More optimized code folding
	Plug 'liuchengxu/vim-which-key'   " Interactive keybinding help
	Plug 'tpope/vim-repeat'           " Repeat more things with `.`
	Plug 'tpope/vim-unimpaired'       " Browse more things with `[]`
	Plug 'junegunn/goyo.vim'          " Center buffer when writing
	" Text editing
	Plug 'andymass/vim-matchup'       " Smarter `%` jumps and highlights
	Plug 'junegunn/vim-slash'         " Smarter `*` jumps and highlights
	Plug 'rhysd/clever-f.vim'         " Smarter `f` jumps and highlights
	Plug 'machakann/vim-sandwich'     " More intuitive `surround` plugin
	Plug 'tpope/vim-commentary'       " Faster (un)commenting with `gc`
	Plug 'junegunn/vim-easy-align'    " Faster code alignment with `ga`
	Plug 'tpope/vim-speeddating'      " Better C-a/C-x actions for dates
	Plug 'guns/vim-sexp'              " Support for Lisp/Sexp slurp/barf.
	" IDE features
	Plug 'dense-analysis/ale'         " Linters and language servers
	Plug 'junegunn/fzf',              { 'do': './install --bin' }
	Plug 'junegunn/fzf.vim'           " Fuzzy finding of everything
	Plug 'Lenovsky/nuake'             " Per-tab pop-up terminal
	Plug 'tpope/vim-fugitive'         " Version control (general)
	Plug 'jreybert/vimagit'           " Version control (commit)
	Plug 'airblade/vim-gitgutter'     " Version control (buffer)
	Plug 'honza/vim-snippets'         " Snippet collection
	Plug 'SirVer/ultisnips'           " Snippet engine
	Plug 'mcchrish/nnn.vim'           " File browser
	" Language support
	Plug 'cespare/vim-toml'           " TOML (syntax)
	Plug 'jceb/vim-orgmode'           " Org (general)
	Plug 'lervag/vimtex'              " TeX (general)
	Plug 'PietroPate/vim-tex-conceal' " TeX (conceal)
	Plug 'plasticboy/vim-markdown'    " MarkDown (general)
	Plug 'tmhedberg/SimpylFold'       " Python (folding)
	Plug 'vim-python/python-syntax'   " Python (syntax)
	Plug 'jmcantrell/vim-virtualenv'  " Python (venv)
	" Miscellaneous
	Plug 'glacambre/firenvim',        { 'do': { _ -> firenvim#install(0) } }
call plug#end()

" Activate colorscheme based on context.
if exists('g:started_by_firenvim')
	set background=light
	silent! colorscheme zellner
else
	set background=dark
	silent! colorscheme moonlight
endif

" #1 Keybindings
" #2 Leader keys
" Spacemacs-like leaders.
let mapleader="\<space>"
let maplocalleader=","

" Spacemacs-like hints.
nnoremap <silent> <leader>      :WhichKey '<space>'<cr>
nnoremap <silent> <localleader> :WhichKey ','<cr>

" Window splits.
map <leader>w <C-w>
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
map <leader>oo :tabedit ~/projects/notes/sintef.org<cr>
map <leader>oO :tabedit ~/projects/notes/personal.org<cr>
map <leader>op :bot split term://htop<cr>i
map <leader>ot :Nuake<cr><C-\><C-n>:set scrolloff=999<cr>:<C-c>
map <leader>oT :terminal<cr>
map <leader>ov <leader>tv
map <leader>oV <leader>tV

" Find files.
map <leader>ff :Files ~/projects/<cr>
map <leader>fF :Files ~/<cr>
map <leader>fb :Buffers<cr>
map <leader>fd :Files ~/.dotfiles/<cr>
map <leader>fD :Files /etc/<cr>
map <leader>fg :GFiles<cr>
map <leader>fG :GFiles?<cr>
map <leader>fn :Files ~/projects/notes/<cr>
map <leader>fp :Files ~/projects/<cr>
map <leader>fr :History<cr>

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
map <leader>ss    :BLines<cr>
map <leader>sn    :cd ~/projects/notes/<cr>:Rg<cr>
map <leader>sp    :GitCd<cr>:Rg<cr>
map <leader>sg    :Ggrep<space>
map <leader>s<cr> :Ggrep <C-R><C-W><cr>

" Manage Vim.
map <leader>qa :qa<cr>
map <leader>qr :source ~/.config/nvim/init.vim<cr>R
map <leader>qi :PlugInstall<cr>
map <leader>qu :PlugUpdate<cr>

" Leader-based speed keys.
map <leader><leader> <leader>fg
map <leader>, <leader>fb
map <leader>. <leader>od
map <leader>/ <leader>sp
map <leader>: :Commands<cr>
map <leader>; :Commands<cr>
map <leader>h :Helptags<cr>

" Non-leader speed keys.
map  ; :
nmap ` <leader>ot
tmap ` <C-\><C-n><leader>ot

" #2 Editor keys
" Better defaults.
nnoremap ' `
nnoremap Y y$
noremap  j gj
noremap  k gk
nnoremap U <C-r>
noremap  R :filetype detect<cr>

" Paging with HJKL.
noremap H zH
noremap L zL
noremap J <C-d>
noremap K <C-u>

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

" Sexp operations.
nmap <M-]> <Plug>(sexp_capture_next_element)
nmap <M-[> <Plug>(sexp_emit_tail_element)
nmap <M-{> <Plug>(sexp_capture_prev_element)
nmap <M-}> <Plug>(sexp_emit_head_element)

" Ergononmic matching jumps.
map <bs> %
map <cr> *

" Ergonomic code folding.
nmap <tab> <Plug>(fold-cycle-open)
nnoremap <S-tab> zm

" Jumping through history.
nnoremap - <C-o>
nnoremap _ <C-i>

" Align expressions.
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)

" Relocated keys.
noremap Q K
noremap M J

" Escape to normal mode with `jk`.
inoremap jk <esc>

" Escape to normal mode in terminals.
augroup terminal_escape
	autocmd!
	autocmd TermOpen * tnoremap <buffer> <esc> <c-\><c-n>
	autocmd FileType fzf tunmap <buffer> <esc>
augroup END


