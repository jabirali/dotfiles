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
	use {'nvim-telescope/telescope.nvim',
		requires={
			{'nvim-lua/popup.nvim'},
			{'nvim-lua/plenary.nvim'},
		}
	}

	-- User interface.
	use {'arcticicestudio/nord-vim',
		config=function()
			vim.g.nord_uniform_diff_background = true
			vim.cmd('colorscheme nord')
		end
	}

	use {'itchyny/lightline.vim',
		setup=function()
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
			    separator = { left = "", right = "" };
			    subseparator = { left = "", right = "" };
			}
		end
	}
end)
