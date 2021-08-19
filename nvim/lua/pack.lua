-- ~/.config/nvim/lua/pack.lua vim: foldmethod=indent foldlevel=1
-- This file invokes Packer for package management, i.e. to install and
-- configure relevant Neovim packages. Run `:PackerSync` after editing.

-- Install and configure packages.
require('packer').startup(function()
	-- Package management.
	use {'wbthomason/packer.nvim'}

	-- Sensible defaults.
	use {'tpope/vim-rsi'}
	use {'wellle/targets.vim'}
	use {'tpope/vim-commentary'}
	use {'tpope/vim-unimpaired'}
	use {'machakann/vim-sandwich'}
	use {'junegunn/vim-slash'}
	use {'bronson/vim-visual-star-search'}

	-- User experience.
	use {'ludovicchabant/vim-gutentags',
		config=function()
			vim.g.gutentags_cache_dir = vim.fn.stdpath('cache') .. '/ctags/'
		end
	}

	-- User interface.
	use {'ishan9299/nvim-solarized-lua',
		config=function()
			vim.o.background = "light"
			vim.cmd('colorscheme solarized-flat')
		end
	}
	
	-- Language support.
	use {'vim-pandoc/vim-pandoc'}
	use {'vim-pandoc/vim-pandoc-syntax'}
	use {'lervag/wiki.vim',
		config=function()
			vim.g.wiki_root = '~/Notes/Wiki'
			vim.g.wiki_filetypes = {'md'}
		end
	}
end)
