" vim: foldmethod=marker 
" ~/.config/nvim/init.vim

" {{{ Plugin management

" Install the manager
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
         https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Install the plugins.
call plug#begin('~/.local/share/nvim/plugins')
  " User interface
  Plug 'Brettm12345/moonlight.vim' " Moonlight colorscheme
  Plug 'Konfekt/FastFold'          " More optimized code folding
  Plug 'liuchengxu/vim-which-key'  " Interactive keybinding help
  Plug 'tpope/vim-repeat'          " Repeat more things with `.`
  " Text editing
  Plug 'arecarn/vim-fold-cycle'    " Org-mode-like fold cycling
  Plug 'andymass/vim-matchup'      " Smarter `%` jumps and highlights
  Plug 'junegunn/vim-slash'        " Smarter `*` jumps and highlights
  Plug 'tpope/vim-speeddating'     " Smarter number inc/dec actions
  "Plug 'justinmk/vim-sneak'        " Smarter and more character jumps
  Plug 'tpope/vim-commentary'      " Smarter (un)commenting with `gc`
  Plug 'machakann/vim-sandwich'    " More intuitive `surround` plugin
  " IDE features
  Plug 'Lenovsky/nuake'            " One pop-up terminal per tab
  Plug 'junegunn/fzf.vim'          " Fuzzy finding of everything
  Plug 'junegunn/fzf',             { 'do': { -> fzf#install() } }
  " Language support
  Plug 'lervag/vimtex'             " *.tex  (general)
  Plug 'PietroPate/vim-tex-conceal' " *.tex (conceal)
  Plug 'jceb/vim-orgmode'          " *.org  (general)
  Plug 'plasticboy/vim-markdown'   " *.md  (syntax, folding)
  Plug 'cespare/vim-toml'          " TOML (syntax)
  Plug 'tmhedberg/SimpylFold'      " Python fold plugin
  "Plug 'vim-ctrlspace/vim-ctrlspace'

" " Plugin: Completion and snippets
" Plug 'Shougo/deoplete.nvim'
" Plug 'roxma/vim-hug-neovim-rpc'
" Plug 'roxma/nvim-yarp'

" " Plugin: Finder, motions, and tags
" Plug 'ctrlpvim/ctrlp.vim'       " Fuzzy file finder
" Plug 'dyng/ctrlsf.vim'          " A very nice search and replace plugin

" " Plugin: Linting, debugging, and code runners
" Plug 'w0rp/ale'

" " Plugin: Version control systems
" Plug 'rbong/vim-flog'
" Plug 'tpope/vim-fugitive'

" " Filetype: python
" Plug 'davidhalter/jedi-vim'     " Python plugin (e.g. completion)
" Plug 'vim-python/python-syntax' " Python syntax plugin

call plug#end()
" }}}

" {{{ Neovim settings
" Color scheme
set termguicolors
colorscheme moonlight

" User interface
set scrolloff=999 
set sidescrolloff=5
set nowrap
set noshowmode
set hidden
set number

" Spell check
set spelllang=en,nb

" Search & replace
set ignorecase
set smartcase
set gdefault
set inccommand=nosplit

" Indentation & folding
set tabstop=4
set shiftwidth=4
set foldlevel=0
set foldmethod=syntax
set conceallevel=2
set concealcursor=nc

" Autocompletion
set wildmode=longest:full,full
set complete+=k,s,d
set completeopt=longest,menu,preview

" Miscellaneous
set virtualedit=block
set clipboard+=unnamedplus
set tildeop
set autochdir
set confirm

" Live substitution

" Confirm quit
set winaltkeys=no

" Substitute g on my default
"set mouse=a


set splitbelow
set splitright

" }}}
" {{{ Plugin settings

" TeX/MD folding

"let g:python_highlight_all = 1
let g:SimpylFold_docstring_preview = 1
let g:vim_markdown_folding_style_pythonic = 1
let g:org_aggressive_conceal = 1
"
"
" let g:tex_stylish = 1
" let g:tex_conceal = ''
" let g:tex_flavor = 'latex'
" let g:tex_isk='48-57,a-z,A-Z,192-255,:'

" let g:vimtex_fold_enabled = 1
" let g:vimtex_quickfix_open_on_warning = 0
" let g:vimtex_index_split_pos = 'below'
" let g:vimtex_toc_hotkeys = {'enabled' : 1}
" let g:vimtex_view_general_viewer = 'evince'

  " let g:vimtex_compiler_progname = 'nvr'

" }}}
" {{{ Autocommands

" Less intrusive `vim-matchup` highlights.
augroup matchup_highlight
  autocmd!
  autocmd ColorScheme * hi MatchParen guifg=none gui=bold
augroup END

augroup active_cursor_line
  autocmd!
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END

augroup emacs_like_exit
	autocmd FileType help noremap <buffer> q :q<cr>
augroup END

" Turn off line numbers in terminals.
"au TermOpen * setlocal listchars= nonumber norelativenumber
" }}}
" {{{ Leader keybindings

" Spacemacs-like leaders.
let mapleader="\<space>"
let maplocalleader="\\"

" Provide interactive hints.
nnoremap <silent> <leader>      :WhichKey '<space>'<cr>
nnoremap <silent> <localleader> :WhichKey ','<cr>

" Window splits.
map <leader>w <C-w>

" Workspaces.
map <leader><tab>n :tabnew<cr>
map <leader><tab>1 :1tabnext<cr>
map <leader><tab>2 :2tabnext<cr>
map <leader><tab>3 :3tabnext<cr>
map <leader><tab>4 :4tabnext<cr>
map <leader><tab>5 :5tabnext<cr>
map <leader><tab>6 :6tabnext<cr>
map <leader><tab>7 :7tabnext<cr>
map <leader><tab>8 :8tabnext<cr>
map <leader><tab>9 :9tabnext<cr>
map <leader><tab>] :tabnext<cr>
map <leader><tab>[ :tabprev<cr>
map ]<tab> :tabnext<cr>
map [<tab> :tabprev<cr>

" Toggle options.
" TODO: Unimpaired?
map <leader>ts :set spell
map <leader>tS :set nospell

" Open applications.
map <leader>ot :bot split term://fish<cr>i
map <leader>of :bot split term://nnn<cr>i
map <leader>op :bot split term://htop<cr>i

" Find files.
map <leader>fg :BCommits<cr>
map <leader>fG :Commits<cr>
map <leader>fr :History<cr>
map <leader>ff :Files ~/projects/<cr>
map <leader>fn :Files ~/projects/notes/<cr>
map <leader>fd :Files ~/.dotfiles<cr>

" Search.
map <leader>ss :BLines<cr>
map <leader>sn :<cr>

" Speed keys.
map <leader>, :Buffers<cr>
map <leader>. :GFiles<cr>
map <leader>h :Helptags<cr>
map <leader>: :Commands<cr>
"map <leader>k :Maps<cr>
"map <leader>/ :Ag<cr>
"map <leader><leader> :Buffers<cr>

" Manage Vim.
map <leader>bd :q<cr>
map <leader>qq :qq<cr>
map <leader>qa :qa<cr>
map <leader>qr :source ~/.config/nvim/init.vim<cr>
map <leader>qu :PlugUpdate<cr>

" }}}
" {{{ Editor keybindings

" Better defaults.
nnoremap ' `
nnoremap Y y$

" Paging with HJKL.
noremap J <C-d>
noremap K <C-u>
noremap H zH
noremap L zL

" Relocated keys.
noremap Q K
noremap M J

" Escape to normal mode in Vim terminals,
" without interfering with FZF closing.
augroup terminal_escape
	autocmd!
	autocmd TermOpen * tnoremap <buffer> <esc> <c-\><c-n>
	autocmd FileType fzf tunmap <buffer> <esc>
augroup END

"tnoremap 

" Escale to normal mode with `jk`.
inoremap jk <esc>


" nnoremap gt :tabnew<cr>
" nnoremap ]t :tabnext<cr>
" nnoremap [t :tabprev<cr>
" nnoremap gw :<cr>
" nnoremap ]w :<cr>
" nnoremap [w :tabprev<cr>

"tmap <C-x> <C-\><C-n>
"tnoremap <C-h> <C-w>h
"tnoremap <C-j> <C-w>j
"tnoremap <C-k> <C-w>k
"tnoremap <C-l> <C-w>l
"noremap <C-h> <C-w>h
"noremap <C-j> <C-w>j
"noremap <C-k> <C-w>k
"noremap <C-l> <C-w>l
"map <tab> %
"map <S-tab> *
"nnoremap <A-j> zo
"nnoremap <A-k> zc

" nmap <tab> za
" nmap <S-tab> zm
"let g:CtrlSpaceDefaultMappingKey = "<leader><leader> "

"map <tab> %
"map <S-tab> *

let g:tex_conceal="abdgm"
let g:fold_cycle_default_mapping = 0 "disable default mappings
nmap <Tab> <Plug>(fold-cycle-open)
nmap <S-Tab> <Plug>(fold-cycle-close)

" Quake-style terminal
nnoremap ` :Nuake<cr>
tnoremap ` <C-\><C-n>:Nuake<cr>
let g:nuake_per_tab = 1
" }}}
