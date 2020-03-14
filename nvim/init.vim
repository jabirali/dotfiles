" ~/.config/nvim/init.vim vim: foldmethod=marker foldmarker="\ #,"###

" #1 Settings
" #2 Builtin
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
set mouse=a
set noshowmode
set nowrap
set scrolloff=999
set shiftwidth=4
set shortmess+=c
set sidescrolloff=5
set signcolumn=no
set smartcase
set showtabline=0
set spelllang=en,nb
set splitbelow
set splitright
set statusline=%#VertSplit#%=
set tabstop=4
set termguicolors
set tildeop
set title
set titlestring=vim:\ %{fnamemodify(substitute(getcwd(),$HOME,'~',''),':t')}
set undofile
set updatetime=4000
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
let g:fzf_history_dir = '~/.local/share/fzf'
let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_italic = 1
let g:magit_default_fold_level = 1
let g:mucomplete#chains = { 'default': ['user', 'ulti', 'omni', 'path', 'dict', 'spel'] }
let g:mucomplete#tab_when_no_results = 0
let g:nnn#replace_netrw = 1
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


" #1 Commands
" #2 New commands
" Define a simple custom folding style. For filetypes that don't have custom
" folding packages loaded, this is much less noisy than the Neovim defaults.
set foldtext=SimpleFoldText()
function! SimpleFoldText()
	return getline(v:foldstart)
endfunction

" Search for a project.
command! -bang Project call fzf#run(fzf#wrap(
			\ 'project',
			\ { 'source':  'ls -d ~/projects/*',
			\   'sink':    'cd',
			\   'options': '--prompt "Project> " -d / --with-nth=-1 --preview="bat --style=plain --color=always {..}/README{.md,.org,.txt,} 2>/dev/null"' },
			\ <bang>0))

" Search Zotero library.
command! -bang Zotero call fzf#run(fzf#wrap(
			\ 'zotero',
			\ { 'source':  'fdfind -t f -e pdf . ~/.zotero/',
			\   'sink':    'silent !zathura --fork',
			\   'options': '--prompt "Zotero> " -m -d / --with-nth=-1' },
			\ <bang>0))

" Jump to the Git project root.
command! GitCd execute 'cd ./'.system('git rev-parse --show-cdup')

" #2 Autocommands
" Normal mode after timeout.
augroup normal_timeout
	autocmd!
	autocmd CursorHoldI * stopinsert
augroup END

" Close pop-ups with q.
augroup quit_like_emacs
	autocmd!
	autocmd BufWinEnter quickfix noremap <buffer> q :silent close<cr>
	autocmd FileType help,man noremap <buffer> q :silent q!<cr>
	autocmd FileType scratch noremap <buffer> <esc> :silent close<cr>
augroup END

" Improve the default highlight colors.
augroup clean_highlights
 	autocmd!
 	" Subtle highlighting of active window.
 	autocmd ColorScheme * hi! InactiveWindow guibg=none
 	autocmd ColorScheme * hi! ActiveWindow guibg='#222526'
	set winhighlight=Normal:ActiveWindow,NormalNC:InactiveWindow
 	" Remove highlighting of active tab.
 	autocmd ColorScheme * hi! link TabLine InactiveWindow
 	autocmd ColorScheme * hi! link TabLineFill InactiveWindow
 	autocmd ColorScheme * hi! TabLineSel guibg=none gui=bold
 	" Tone down too heavy default highlighting.
	autocmd ColorScheme * hi! link Folded Comment
 	autocmd ColorScheme * hi! LineNr guibg=none
 	autocmd ColorScheme * hi! link CursorLineNr LineNr
	" More consistent highlighting.
 	autocmd ColorScheme * hi! link MatchParen Search
 	autocmd ColorScheme * hi! link CleverFDefaultLabel Search
 	" Simplify the LSP diagnostics view.
 	autocmd ColorScheme * hi! link LspDiagnosticsUnderlineError SpellCap
 	autocmd ColorScheme * hi! link LspDiagnosticsUnderlineWarning SpellBad
 	autocmd ColorScheme * hi! link LspDiagnosticsUnderlineHint SpellRare
 	autocmd ColorScheme * hi! link LspDiagnosticsUnderlineInformation SpellRare
augroup END

" Improve the default terminal settings.
augroup terminal_settings
	autocmd!
	" Keep the terminals simple and clean.
	autocmd TermOpen  * setlocal scrolloff=0 nonumber norelativenumber
	autocmd TermLeave * setlocal scrolloff=999
	" Escape to normal mode in terminals.
	autocmd TermOpen * tnoremap <buffer> <esc> <c-\><c-n>
	autocmd FileType fzf tunmap <buffer> <esc>
	" Navigate prompts like sections.
	autocmd TermOpen * nnoremap <buffer> [[ ?❯<cr>:nohlsearch<cr>:<esc>
	autocmd TermOpen * nnoremap <buffer> ]] /❯<cr>:nohlsearch<cr>:<esc>
augroup END

" Python integration.
augroup python_settings
    autocmd!
	autocmd BufWritePre *.py execute ':Black'
	autocmd FileType python runtime zepl/contrib/python.vim
	autocmd FileType python let b:repl_config =
				\ { 'cmd': 'fish -c python3',
				\   'formatter': function("zepl#contrib#python#formatter") }
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
	Plug 'morhetz/gruvbox'              " Gruvbox colorscheme
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
	Plug 'axvr/zepl.vim'                " Minimal REPL integration
	Plug 'honza/vim-snippets'           " Snippets collection
	Plug 'sirver/ultisnips'             " Snippets engine
	" Miscellaneous
	Plug 'mtth/scratch.vim'             " Scratch buffer
	Plug 'mcchrish/nnn.vim'             " File browser
	Plug 'adborden/vim-notmuch-address' " Mail address
	" Python
	Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins'}
	Plug 'kalekundert/vim-coiled-snake'
	Plug 'jmcantrell/vim-virtualenv'
	Plug 'psf/black', {'tag': '19.10b0'}
	" LaTeX
	Plug 'lervag/vimtex'
	Plug 'KeitaNakamura/tex-conceal.vim'
	" Markup
	Plug 'vim-pandoc/vim-pandoc-syntax'
	Plug 'vim-pandoc/vim-pandoc'
	Plug 'jceb/vim-orgmode'
	" Config
	Plug 'cespare/vim-toml'
call plug#end()

" #2 Miscellaneous
" Load LSP support.
luafile ~/.config/nvim/lsp.lua

" Load color scheme.
silent! colorscheme gruvbox


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
" nnoremap <leader><tab>1 :1tabnext<cr>
" nnoremap <leader><tab>2 :2tabnext<cr>
" nnoremap <leader><tab>3 :3tabnext<cr>
" nnoremap <leader><tab>4 :4tabnext<cr>
" nnoremap <leader><tab>5 :5tabnext<cr>
" nnoremap <leader><tab>6 :6tabnext<cr>
" nnoremap <leader><tab>7 :7tabnext<cr>
" nnoremap <leader><tab>8 :8tabnext<cr>
" nnoremap <leader><tab>9 :9tabnext<cr>
" nnoremap <leader><tab>d :tabclose<cr>
" nnoremap <leader><tab>n :tabnew<cr>
" nnoremap <leader><tab>] :tabnext<cr>
" nnoremap <leader><tab>[ :tabprev<cr>
" nnoremap ]<tab> :tabnext<cr>
" nnoremap [<tab> :tabprev<cr>

" Toggle stuff.
nmap <leader>t yo
nmap <leader>tg :Goyo<cr>
nmap <leader>tv :VirtualEnvActivate<space>
nmap <leader>tV :VirtualEnvDeactivate<cr>

" Open stuff.
" nmap <leader>od :NnnPicker '%:p:h'<cr>
" nmap <leader>ol :bot lwindow<cr>
" nmap <leader>oo :tabedit ~/notes/sintef.org<cr>
" nmap <leader>oO :tabedit ~/notes/personal.org<cr>
" nmap <leader>op :bot split term://htop<cr>i
" nmap <leader>os :Scratch<cr>
" nmap <leader>oS :Scratch!<cr>
" nmap <leader>ot :Nuake<cr>
" nmap <leader>oT :terminal<cr>
" nmap <leader>ov <leader>tv
" nmap <leader>oV <leader>tV
" nmap <leader>oq :bot cwindow<cr>
" nmap <leader>oz :top 10 Repl<cr>

" Find files.
nmap <leader>ff :Files %:p:h<cr>
nmap <leader>fo :Files ~/onedrive/<cr>
nmap <leader>fF :Files ~/<cr>
nmap <leader>fb :Buffers<cr>
nmap <leader>fd :Files ~/.config/<cr>
nmap <leader>fD :Files /etc/<cr>
nmap <leader>fg :GFiles<cr>
nmap <leader>fG :GFiles?<cr>
nmap <leader>fn :Files ~/notes/<cr>
nmap <leader>fr :History<cr>
nmap <leader>fz :Zotero!<cr>

" Version control.
nmap <leader>gg :MagitOnly<cr>
nmap <leader>gb :GBlame<cr>
nmap <leader>gc :BCommits<cr>
nmap <leader>gC :Commits<cr>
nmap <leader>gd :Git difftool<cr>
nmap <leader>gD :Gdelete<space>
nmap <leader>gf :Gfetch<cr>
nmap <leader>gl :Glog<cr>
nmap <leader>gm :Git mergetool<cr>
nmap <leader>gp :Gpush<cr>
nmap <leader>gR :Gmove<space>
nmap <leader>gs <Plug>(GitGutterStageHunk)
nmap <leader>gu <Plug>(GitGutterUndoHunk)
nmap <leader>gz :GitGutterSignsToggle<cr>:GitGutterFold<cr>

" Search content.
nmap <leader>ss :BLines<cr>
nmap <leader>sr :%s/\<<C-r><C-w>\>//c<left><left>
nmap <leader>sg :Ggrep <c-r><c-w><cr>
nmap <leader>sn :NV<cr>
nmap <leader>sp :GitCd<cr>:Rg<cr>

" Manage Vim.
nmap <leader>qa :qa<cr>
nmap <leader>qr :source ~/.config/nvim/init.vim<cr>
nmap <leader>qi :PlugInstall<cr>
nmap <leader>qu :PlugUpdate<cr>

" Leader-based speed keys.
nmap <leader><leader> :Buffers<cr>
nmap <leader><tab> <c-^>
nmap <leader><cr> :terminal<cr>
nmap <leader>, <leader>fb
nmap <leader>. <leader>ff
nmap <leader>/ <leader>sp
nmap <leader>p :Project!<cr>
nmap <leader>o :GFiles!<cr>
nmap <leader>: :Commands<cr>
nmap <leader>h :Helptags<cr>

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
nnoremap ^ 0
nnoremap Y y$
nnoremap U <c-r>
noremap <c-j> J
noremap <c-l> :nohlsearch<cr><c-l>
noremap <expr> j (v:count? 'j' : 'gj')
noremap <expr> k (v:count? 'k' : 'gk')

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

" Open externally.
nnoremap gx :silent !xdg-open "<cfile>:p"<cr>

" Align expressions.
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)

" Inspired by Firefox.
nnoremap <c-r> :e<cr>

" Inspired by i3.
nnoremap <m-h> <c-w>h 
nnoremap <m-j> <c-w>j 
nnoremap <m-k> <c-w>k 
nnoremap <m-l> <c-w>l 

" TeX bindings.
nnoremap <localleader>lt :call vimtex#fzf#run()<cr>

" Completion.
inoremap <silent> <expr> <cr> mucomplete#ultisnips#expand_snippet("\<cr>")

