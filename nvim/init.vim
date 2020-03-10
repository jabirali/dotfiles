" ~/.config/nvim/init.vim vim: foldmethod=marker foldmarker="\ #,"###

" #1 Settings
" #2 Builtin
set autochdir
set background=dark
set clipboard+=unnamedplus
set completeopt=longest,menuone,noinsert
set concealcursor=nc
set conceallevel=2
set confirm
set fillchars=fold:\ ,stl:\ ,stlnc:\ ,vert:\ ,eob:\ ,
set foldlevel=0
set foldmethod=syntax
set gdefault
set guifont=Iosevka\ SS09\ Light:h16
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

" #2 Plugins
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
let g:mucomplete#chains = { 'default': ['ulti', 'omni', 'path', 'dict', 'spel'] }
let g:mucomplete#tab_when_no_results = 0
let g:nnn#replace_netrw = 1
let g:nuake_per_tab = 1
let g:nuake_position = 'top'
let g:nuake_size = 0.20
let g:nv_search_paths = ['~/notes']
let g:org_aggressive_conceal = 1
let g:pandoc#folding#fdc = 0
let g:scratch_insert_autohide = 0
let g:semshi#mark_selected_nodes = 0
let g:tex_conceal = 'abdgm'
let g:UltiSnipsExpandTrigger = '<noop>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:vimtex_compiler_progname = 'nvr'
let g:vimtex_fold_enabled = 1
let g:vimtex_view_method = 'zathura'
let loaded_netrwPlugin = 1


" #1 Commands
" #2 New commands
" Define a simple custom folding style. For filetypes that don't have custom
" folding packages loaded, this is much less noisy than the Neovim defaults.
set foldtext=SimpleFoldText()
function! SimpleFoldText()
	return getline(v:foldstart)
endfunction

" Search through Zotero library.
command! -bang Zotero call fzf#run(fzf#wrap(
			\ 'zotero',
			\ { 'source':  'fdfind -t f -e pdf . ~/.zotero/',
			\   'sink':    'silent !zathura --fork',
			\   'options': '-m -d / --with-nth=-1' },
			\ <bang>0))

" Jump to the Git project root.
command! GitCd execute 'cd ./'.system('git rev-parse --show-cdup')

" #2 Autocommands
" Close pop-ups with escape.
augroup quit_like_doom
	autocmd!
	autocmd BufWinEnter quickfix noremap <buffer> <esc> :silent q<cr>
	autocmd FileType help,man,scratch noremap <buffer> <esc> :silent q<cr>
	autocmd TermOpen * noremap <buffer> <esc> <c-\><c-n>:silent close<cr>
augroup END

" Keep the terminals simple and clean.
augroup terminal_settings
	autocmd!
	autocmd TermOpen  * setlocal scrolloff=0 nonumber norelativenumber
	autocmd TermLeave * setlocal scrolloff=999
augroup END

" Improve the default highlight colors.
augroup clean_highlights
	autocmd!
	" Suddle highlighting of active window.
	autocmd ColorScheme * hi! InactiveWindow guibg=none
	autocmd ColorScheme * hi! ActiveWindow guibg='#23253a'
	set winhighlight=Normal:ActiveWindow,NormalNC:InactiveWindow
	" Remove highlighting of active tab.
	autocmd ColorScheme * hi! TabLine guibg=none
	autocmd ColorScheme * hi! TabLineFill guibg=none
	autocmd ColorScheme * hi! TabLineSel guibg=none gui=bold
	" Tone down too heavy default highlighting.
	autocmd ColorScheme * hi! LineNr guibg=none
	autocmd ColorScheme * hi! link CursorLineNr LineNr
	autocmd ColorScheme * hi! Conceal guifg=none guibg=none
	autocmd ColorScheme * hi! MatchParen guifg=none gui=bold
	autocmd ColorScheme * hi! CleverFDefaultLabel guifg=white gui=bold
	autocmd ColorScheme * hi! SpellBad guifg=red gui=none
	" Simplify the LSP diagnostics view.
	autocmd ColorScheme * hi! link LspDiagnosticsUnderlineError LspDiagnosticsError
	autocmd ColorScheme * hi! link LspDiagnosticsUnderlineHint LspDiagnosticsHint
	autocmd ColorScheme * hi! link LspDiagnosticsUnderlineInformation LspDiagnosticsInformation
	autocmd ColorScheme * hi! link LspDiagnosticsUnderlineWarning LspDiagnosticsWarning
augroup END

" Automatic triggering of plugins.
augroup plugin_hooks
	autocmd!
	autocmd BufWritePre *.py execute ':Black'
augroup END


" #1 External
" #2 Plugin manager
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
	silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
				\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * nested PlugInstall --sync | source $MYVIMRC
endif

" #2 Load plugins
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
    Plug 'wlemuel/vim-tldr',            {'do': ':TldrUpdateDocs'}
	" Version control
	Plug 'tpope/vim-fugitive'           " Version control (general)
	Plug 'jreybert/vimagit'             " Version control (commit)
	Plug 'airblade/vim-gitgutter'       " Version control (buffer)
	" IDE features
	Plug 'neovim/nvim-lsp'              " Setup built-in language client
	Plug 'lifepillar/vim-mucomplete'    " Setup built-in autocompletion
	Plug 'honza/vim-snippets'           " Snippets collection
	Plug 'sirver/ultisnips'             " Snippets engine
	" Miscellaneous
	Plug 'Lenovsky/nuake'               " Quake terminal
	Plug 'mtth/scratch.vim'             " Scratch buffer
	Plug 'mcchrish/nnn.vim'             " File browser
	" Python
	Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins'}
	Plug 'kalekundert/vim-coiled-snake'
	Plug 'jmcantrell/vim-virtualenv'
	Plug 'psf/black', {'tag': '19.10b0'}
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

" #2 Miscellaneous
" Load color scheme.
silent! colorscheme moonlight

" Load LSP settings.
luafile ~/.config/nvim/lsp.lua


" #1 Keybindings
" #2 Leader keys
" Spacemacs-like leaders.
let mapleader="\<space>"
let maplocalleader=","

" Spacemacs-like hints.
nnoremap <silent> <leader>      :WhichKey '<space>'<cr>
nnoremap <silent> <localleader> :WhichKey ','<cr>

" Window splits.
nnoremap <leader>w <c-w>
nnoremap <leader>wd :close<cr>

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
map <leader>oo :tabedit ~/notes/sintef.org<cr>
map <leader>oO :tabedit ~/notes/personal.org<cr>
map <leader>op :bot split term://htop<cr>i
map <leader>ot :Nuake<cr>
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
map <leader>fn :Files ~/notes/<cr>
map <leader>fp :Files ~/projects/<cr>
map <leader>fr :History<cr>
map <leader>fz :Zotero!<cr>

" Version control.
map <leader>gg :MagitOnly<cr>
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
map <leader>s* :Ggrep <c-r><c-w><cr>
map <leader>sg :Ggrep<space>
map <leader>sn :NV<cr>
map <leader>sp :GitCd<cr>:Rg<cr>

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
" LSP bindings.
set omnifunc=v:lua.vim.lsp.omnifunc
nnoremap <silent> g=    <cmd>lua vim.lsp.buf.formatting()<cr>
nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<cr>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<cr>
nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<cr>
nnoremap <silent> gR    <cmd>lua vim.lsp.buf.rename()<cr>
nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<cr>
inoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<cr>
nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.hover()<cr><cmd>lua vim.lsp.util.show_line_diagnostics()<cr>

" Better defaults.
nnoremap ; :
nnoremap ' `
nnoremap 0 ^
nnoremap Y y$
noremap  j gj
noremap  k gk
nnoremap U <c-r>
noremap <c-j> J

" Paging with HJKL.
noremap H zH
noremap L zL
noremap J <c-d>
noremap K <c-u>

" Tab fold and indent.
nmap <tab> <Plug>(fold-cycle-open)
nnoremap <s-tab> zm
vnoremap <tab>   >gv
vnoremap <s-tab> <gv

" Align expressions.
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)

" Open terminals.
nnoremap gt :Nuake<cr>
vnoremap gt y:Nuake<cr><c-\><c-n>pA<cr><cr><c-\><c-n>
nnoremap gT :terminal<cr>

" Inspired by Firefox.
nnoremap <c-r> :e<cr>

" Inspired by i3.
nnoremap <m-h> <c-w>h 
nnoremap <m-j> <c-w>j 
nnoremap <m-k> <c-w>k 
nnoremap <m-l> <c-w>l 

" Escape to normal mode in terminals.
augroup terminal_escape
	autocmd!
	autocmd TermOpen * tnoremap <buffer> <esc> <c-\><c-n>
	autocmd FileType fzf tunmap <buffer> <esc>
augroup END

" TeX bindings.
nnoremap <localleader>lt :call vimtex#fzf#run()<cr>

" Completion.
inoremap <silent> <expr> <cr> mucomplete#ultisnips#expand_snippet("\<cr>")
