" Options:
    " System integration.
    set mouse+=a
    set clipboard=unnamed
    set undofile

    " Buffers and splits.
    set hidden
    set splitbelow
    set splitright

    " Tab settings.
    set tabstop=4
    set softtabstop=4
    set shiftwidth=4

    " Search settings.
    set smartcase
    set ignorecase
    set incsearch
    set inccommand=nosplit

    " Word wrapping.
    set wrap
    set linebreak
    set breakindent

    " Code folding.
    set foldminlines=0
    set foldmethod=syntax
    set fillchars=fold:\ 

    " Miscellaneous.
    set spelllang="en,nb"
    set virtualedit="block"

" Keybindings:
    " Move by visible lines.
    map j gj
    map k gk
    
    " Org-mode-like folding.
    if exists('g:vscode')
        nnoremap <S-Tab> <Cmd>call VSCodeNotify('editor.foldAll')<CR>
        nnoremap <Tab>   <Cmd>call VSCodeNotify('editor.toggleFold')<CR>
    else
        nnoremap <S-Tab> zM
        nnoremap <Tab>   za
    endif
    
    " Commentary-like commenting.
    if exists('g:vscode')
        xmap gc  <Plug>VSCodeCommentary
        nmap gc  <Plug>VSCodeCommentary
        omap gc  <Plug>VSCodeCommentary
        nmap gcc <Plug>VSCodeCommentaryLine
    endif