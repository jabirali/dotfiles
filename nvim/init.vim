" ~/.config/nvim/init.vim vim: foldmethod=marker foldmarker="\ #,"###
" #1 Settings
" #2 Builtin
set background=light
set completeopt=longest,menuone,noinsert
set autochdir
set confirm
set fillchars=fold:\ ,stl:-,stlnc:-,eob:\ ,
set list
set listchars=tab:╎\ ,
set foldlevel=0
set foldmethod=syntax
set gdefault
set hidden
set ignorecase
set inccommand=nosplit
set laststatus=0
set mouse=a
set noshowmode
set nowrap
set noruler
set scrolloff=999
set shiftwidth=4
set shortmess+=c
set sidescrolloff=5
set signcolumn=yes
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
let g:fzf_history_dir = '~/.local/share/fzf'
let g:gitgutter_map_keys = 0
let g:gitgutter_sign_added = '│'
let g:gitgutter_sign_modified = '│'
let g:gitgutter_sign_modified_removed = '│'
let g:gitgutter_sign_removed = '_'
let g:gitgutter_sign_removed_first_line = '‾'
let g:LspDiagnosticsErrorSign = '!'
let g:LspDiagnosticsWarningSign = '·'
let g:LspDiagnosticsInformationSign = '·'
let g:LspDiagnosticsHintSign = '·'
let g:magit_default_fold_level = 1
let g:mucomplete#chains = { 'default': ['user', 'omni', 'path', 'dict', 'spel'] }
let g:mucomplete#tab_when_no_results = 0
let g:nnn#replace_netrw = 1
let g:nnn#set_default_mappings = 0
let g:pandoc#folding#fdc = 0
let g:pandoc#folding#fold_fenced_codeblocks = 1
let g:pandoc#syntax#style#underline_special = 0
let g:scratch_insert_autohide = 0
let g:tmux_navigator_no_mappings = 1
let g:vimtex_compiler_progname = 'nvr'
let g:vimtex_fold_enabled = 1
let g:vimtex_view_method = 'zathura'
let g:wiki_filetypes = ['md']
let g:wiki_root = '~/Documents/personal-notes/'
let g:wiki_link_target_type = 'md'
let g:wiki_zotero_root = '~/snap/zotero-snap'
let g:wiki_mappings_use_defaults = 0

" #2 Envvars
let $BAT_THEME = 'ansi-dark'

" #1 Commands
" #2 New commands
" Define a simple custom folding style. For filetypes that don't have custom
" folding packages loaded, this is much less noisy than the Neovim defaults.
set foldtext=SimpleFoldText()
function! SimpleFoldText()
	return getline(v:foldstart)
endfunction

" Jump to the Git project root.
command! GitCd execute 'cd ./'.system('git rev-parse --show-cdup')

" #2 Autocommands
" Normal mode after timeout.
" Close pop-ups with q.
augroup quit_like_emacs
	autocmd!
	autocmd BufWinEnter quickfix noremap <buffer> q :silent close<cr>
	autocmd FileType help,man noremap <buffer> q :silent q!<cr>
	autocmd FileType scratch noremap <buffer> <esc> :silent close<cr>
augroup END

" Some apps require buffer deletion.
augroup delete_on_quit
	autocmd!
	autocmd FileType gitcommit,gitrebase,gitconfig set bufhidden=delete
augroup END

" Improve the default highlight colors.
augroup clean_highlights
 	autocmd!
 	" Simplify highlighting of active tab/window.
 	autocmd ColorScheme * hi! TabLine guibg=none
 	autocmd ColorScheme * hi! TabLineFill guibg=none
 	autocmd ColorScheme * hi! TabLineSel guibg=none gui=bold
	autocmd ColorScheme * hi! VertSplit guifg='#d7d7af' guibg=none
 	" Tone down too heavy default highlighting.
	autocmd ColorScheme * hi! SignColumn guibg=none
	autocmd ColorScheme * hi! LineNr guibg=none
	autocmd ColorScheme * hi! link Folded Comment
	" More consistent highlighting.
 	autocmd ColorScheme * hi! link MatchParen Cursor
 	autocmd ColorScheme * hi! link CleverFDefaultLabel Search
 	" Simplify the LSP diagnostics view.
 	autocmd ColorScheme * hi! link LspDiagnosticsUnderlineError SpellCap
 	autocmd ColorScheme * hi! link LspDiagnosticsUnderlineWarning SpellBad
 	autocmd ColorScheme * hi! link LspDiagnosticsUnderlineHint SpellRare
 	autocmd ColorScheme * hi! link LspDiagnosticsUnderlineInformation SpellRare
augroup END

" Python integration.
augroup python_settings
    autocmd!
	autocmd BufWritePre *.py execute ':Black'
augroup END

" Cleaner folding syntax.
augroup fold_settings
	autocmd!
	autocmd BufEnter *.md,*.tex setlocal foldtext=SimpleFoldText()
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
	Plug 'romainl/flattened'              " Solarized colorscheme
	Plug 'arecarn/vim-fold-cycle'         " Org-like recursive folding
	Plug 'Konfekt/FastFold'               " More optimized code folding
	Plug 'liuchengxu/vim-which-key'       " Interactive keybinding help
	Plug 'tpope/vim-repeat'               " Repeat more things with `.`
	Plug 'tpope/vim-unimpaired'           " Browse more things with `[]`
	" Text editing
	Plug 'andymass/vim-matchup'           " Smarter `%` jumps and highlights
	Plug 'junegunn/vim-slash'             " Smarter `*` jumps and highlights
	Plug 'rhysd/clever-f.vim'             " Smarter `f` jumps and highlights
	Plug 'machakann/vim-sandwich'         " More intuitive `surround` plugin
	Plug 'tpope/vim-commentary'           " Faster (un)commenting with `gc`
	Plug 'junegunn/vim-easy-align'        " Faster code alignment with `ga`
	Plug 'tpope/vim-speeddating'          " Better c-a/c-x actions for dates
	" Searching
	Plug 'junegunn/fzf',                  {'do': './install --bin'}
	Plug 'junegunn/fzf.vim'               " Fuzzy finding of everything
	Plug 'lervag/wiki.vim'                " Wiki features in notes
	Plug 'wlemuel/vim-tldr',              {'do': ':TldrUpdateDocs'}
	" Version control
	Plug 'tpope/vim-fugitive'             " Version control (general)
	Plug 'jreybert/vimagit'               " Version control (commit)
	Plug 'airblade/vim-gitgutter'         " Version control (buffer)
	" IDE features
	Plug 'neovim/nvim-lsp'                " Setup built-in language client
	Plug 'lifepillar/vim-mucomplete'      " Setup built-in autocompletion
	" Miscellaneous
	Plug 'jabirali/vim-tmux-yank'         " Tmux clipboard
	Plug 'christoomey/vim-tmux-navigator' " Tmux consistency
	Plug 'tpope/vim-rsi'                  " Readline consistency
	Plug 'tpope/vim-obsession'            " Session backups
	Plug 'mtth/scratch.vim'               " Scratch buffer
	Plug 'mcchrish/nnn.vim'               " File browser
	" Python
	Plug 'vim-python/python-syntax'
	Plug 'kalekundert/vim-coiled-snake'
	Plug 'psf/black', {'tag': '19.10b0'}
	" LaTeX
	Plug 'lervag/vimtex'
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
set background=light
silent! colorscheme flattened_light

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

" Toggle stuff.
nmap <leader>t yo

" Find files.
nmap <leader>ff :Files %:p:h<cr>
nmap <leader>fF :Files ~/<cr>
nmap <leader>fb :Buffers<cr>
nmap <leader>fd :Files ~/.config/<cr>
nmap <leader>fD :Files /etc/<cr>
nmap <leader>fg :GFiles<cr>
nmap <leader>fG :GFiles?<cr>
nmap <leader>fr :History<cr>

" Version control.
nmap <leader>gg :MagitOnly<cr>
nmap <leader>gb :GBlame<cr>
nmap <leader>gc :BCommits<cr>
nmap <leader>gC :Commits<cr>
nmap <leader>gd :Git difftool<cr>
nmap <leader>gf :Gfetch<cr>
nmap <leader>gl :Glog<cr>
nmap <leader>gm :Git mergetool<cr>
nmap <leader>gp :Gpush<cr>
nmap <leader>gs <Plug>(GitGutterStageHunk)
nmap <leader>gu <Plug>(GitGutterUndoHunk)
nmap <leader>gz :GitGutterSignsToggle<cr>:GitGutterFold<cr>

" Search content.
nmap <leader>ss :BLines<cr>
nmap <leader>sr :%s/\<<C-r><C-w>\>//c<left><left>
nmap <leader>sg :Ggrep <c-r><c-w><cr>
nmap <leader>sp :GitCd<cr>:Rg<cr>

" Manage Vim.
nmap <leader>E :qa<cr>
nmap <leader>C :source ~/.config/nvim/init.vim<cr>

" Leader-based speed keys.
nmap <leader><tab> :Buffers<cr>
 map <leader>? :Helptags<cr>
nmap <leader>o :GFiles!<cr>

" #2 Editor keys
" LSP bindings.
set omnifunc=v:lua.vim.lsp.omnifunc
nnoremap <silent> g=    <cmd>lua vim.lsp.buf.formatting()<cr>
nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<cr>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<cr>
nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<cr>
nnoremap <silent> gR    <cmd>lua vim.lsp.buf.rename()<cr>
nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<cr>
inoremap <silent> <c-_> <cmd>lua vim.lsp.buf.signature_help()<cr>
nnoremap <silent> <c-_> <cmd>lua vim.lsp.buf.hover()<cr>
nnoremap <silent> <leader><leader> <cmd>lua vim.lsp.util.show_line_diagnostics()<cr>

" Better defaults.
noremap ; :
nnoremap ' `
nnoremap 0 ^
nnoremap ^ 0
nnoremap Y y$
nnoremap U <c-r>
noremap j gj
noremap k gk
noremap <c-l> :nohlsearch<cr><c-l>
cnoremap <C-r> <C-e><C-u>History:<cr>

" Search and replace.
nnoremap S :%s/
vnoremap S :s/

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

" Tmux integration.
noremap <silent> <m-h> :TmuxNavigateLeft<cr>
noremap <silent> <m-j> :TmuxNavigateDown<cr>
noremap <silent> <m-k> :TmuxNavigateUp<cr>
noremap <silent> <m-l> :TmuxNavigateRight<cr>

" Window splitting.
noremap <silent> <leader>h :leftabove vsplit<cr>
noremap <silent> <leader>j :below split<cr>
noremap <silent> <leader>k :above split<cr>
noremap <silent> <leader>l :rightbelow vsplit<cr>

" TeX bindings.
nnoremap <localleader>lt :call vimtex#fzf#run()<cr>
