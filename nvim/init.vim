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

" Load language server support via `nvim-lsp`.
luafile ~/.config/nvim/lsp.lua

" Load color scheme.
set background=dark
silent! colorscheme base16-material-palenight

