-- ~/.config/nvim/lua/pack.lua:
-- This file invokes Packer for package management, i.e. to install and
-- configure relevant Neovim packages. Run `:PackerSync` after editing.

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
