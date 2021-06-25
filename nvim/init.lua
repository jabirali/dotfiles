-- Lua-based config file for the [Neovim][1] text editor. Based on Michael
-- Lingelbach's [defaults.nvim][2] and Wil Thomason's [packer.nvim][3].
--
-- [1]: https://neovim.io/
-- [2]: https://github.com/mjlbach/defaults.nvim
-- [3]: https://github.com/wbthomason/packer.nvim

-- Bootstrap the package manager.
local packer_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
if vim.fn.empty(vim.fn.glob(packer_path)) > 0 then
	vim.api.nvim_command('!git clone https://github.com/wbthomason/packer.nvim ' .. packer_path)
end

-- Install and configure packages.
require('packer').startup(function()
	-- Fast and native package manager.
	use {'wbthomason/packer.nvim'}

	-- Extensions to the Vim editing language.
	use {'inkarkat/vim-visualrepeat'}
	use {'machakann/vim-sandwich'}
	use {'tpope/vim-commentary'}
	
	-- User experience enhancements.
	use {'tpope/vim-rsi'}
	use {'junegunn/vim-slash'}
	use {'ludovicchabant/vim-gutentags', config=function()
		vim.g.gutentags_cache_dir = vim.fn.stdpath('cache') .. '/ctags/'
	end}
	use {'nvim-telescope/telescope.nvim', requires={{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}}

	-- User interface theming.
	use {'arcticicestudio/nord-vim', config=function()
		vim.g.nord_uniform_diff_background = true
		vim.cmd('colorscheme nord')
	end}

	use {'itchyny/lightline.vim', setup=function()
		vim.o.showmode = false
		vim.g.lightline = {
			colorscheme = 'nord';
			active = {
				left  = { {'filename'} },
				right = { {'lineinfo'} },
			};
			inactive = {
				left  = { {'filename'} },
				right = { },
			};
		      component_function = { gitbranch = 'fugitive#head', };
		      separator = { left = "", right = "" };
		      subseparator = { left = "", right = "" };
		}
	end}
end)

-- Short-hand notation for maps.
local function map(mode, keys, maps, opts)
	local options = {noremap=true, silent=true}
	if opts then
		options = vim.tbl_extend('force', options, opts)
	end
	vim.api.nvim_set_keymap(mode, keys, maps, options)
end

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
