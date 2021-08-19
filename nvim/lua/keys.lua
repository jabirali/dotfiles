-- ~/.config/nvim/lua/keys.lua:
-- This defines a function `map` that acts similar to the VimScript commands
-- `nnoremap` and friends, and uses this to define custom Neovim keybindings.

-- Simplify mapping of keys.
local function map(mode, keys, maps, opts)
	local options = {noremap=true, silent=true}
	if opts then
		options = vim.tbl_extend('force', options, opts)
	end
	vim.api.nvim_set_keymap(mode, keys, maps, options)
end

-- Use ⌃C to exit insert mode. This keybinding is backwards compatible with
-- most terminals and Vim emulations, but this overriding removes the minor
-- syntactic differences documented in `:he i_CTRL-c`.
map('i', '<C-c>', '<esc>')

-- Use space as the leader key.
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
map('', '<space>', '<nop>')

-- Sensible defaults.
map('n', 'Y', 'y$')
map('n', 'k', "v:count == 0 ? 'gk' : 'k'", {expr=true})
map('n', 'j', "v:count == 0 ? 'gj' : 'j'", {expr=true})

-- Leader keybindings.
map('n', '<leader><space>', [[<cmd>lua require('telescope.builtin').buffers()<cr>]])
map('n', '<leader>o', [[<cmd>lua require('telescope.builtin').find_files()<cr>]])
map('n', '<leader>]', [[<cmd>lua require('telescope.builtin').tags()<cr>]])
map('n', '<leader>/', [[<cmd>lua require('telescope.builtin').live_grep()<cr>]])
