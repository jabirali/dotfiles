" ~/.config/nvim/init.vim vim: foldmethod=marker foldmarker="\ #,"###

" I'm in the process of migrating from `vim-plug` to `packadd`, since the
" latter is built into `nvim`, and also works well with Git submodules. Both
" of these enhance portability: you can have a standard `nvim` on a different
" computer, do a `git clone --recursive`, and get all your plugins with you.
" Since my `init.vim` has accumulated a lot of keybindings and plugins that
" I don't actively use, this is also a nice opportunity to clean up a bit:
" I've disabled everything, and will slowly turn on again the stuff I use.

" Load plugins from the Git submodules in `pack`.
packloadall

" Load color scheme.
set background=dark
colorscheme base16-material-palenight

" Load LSP support via `nvim-lsp`.
luafile ~/.config/nvim/lsp.lua

" Enable LSP keybindings.
nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<cr>
nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<cr>
nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<cr>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<cr>
nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<cr>
nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<cr>
nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<cr>
nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<cr>
nnoremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<cr>

" Enable LSP completion.
autocmd Filetype python setlocal omnifunc=v:lua.vim.lsp.omnifunc

" Built-in settings.
set autochdir
set expandtab
set fillchars=fold:\ ,
set foldmethod=syntax

" Define a simple custom folding style. For filetypes that don't have custom
" folding packages loaded, this is much less noisy than the Neovim defaults.
set foldtext=SimpleFoldText()
function! SimpleFoldText()
	return getline(v:foldstart)
endfunction
