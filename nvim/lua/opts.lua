-- ~/.config/nvim/lua/opts.lua:
-- This overrides basic built-in Neovim configuration options.

-- System integration.
vim.o.mouse = "a"
vim.o.clipboard = "unnamed"
vim.o.termguicolors = true
vim.o.undofile = true

-- Buffers and splits.
vim.o.hidden = true
vim.o.splitbelow = true
vim.o.splitright = true

-- Tab settings.
vim.o.tabstop = 4
vim.o.softtabstop = 4
vim.o.shiftwidth = 4

-- Search settings.
vim.o.smartcase = true
vim.o.ignorecase = true
vim.o.incsearch = true
vim.o.inccommand = "nosplit"

-- Word wrapping.
vim.o.wrap = true
vim.o.linebreak = true
vim.o.breakindent = true

-- Code folding.
vim.o.foldminlines = 0
vim.o.foldmethod = "syntax"
vim.o.fillchars = "fold: "

-- Miscellaneous.
vim.o.spelllang = "en,nb"
vim.o.virtualedit = "block"
vim.o.statusline = " %<%f%=%P "
