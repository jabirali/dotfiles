-- Core settings.
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
vim.o.shiftwidth = 4
vim.o.tabstop = 4
vim.o.wrap = false

-- Bootstrap
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system {
		'git',
		'clone',
		'--filter=blob:none',
		'https://github.com/folke/lazy.nvim.git',
		'--branch=stable',
		lazypath,
	}
end
vim.opt.rtp:prepend(lazypath)

-- Package management.
require('lazy').setup({
	-- Language servers.
	{
		'VonHeikemen/lsp-zero.nvim',
		branch = 'v2.x',
		dependencies = {
			{ 'neovim/nvim-lspconfig' },
			{ 'williamboman/mason.nvim' },
			{ 'williamboman/mason-lspconfig.nvim' },
			{ 'hrsh7th/nvim-cmp' },
			{ 'hrsh7th/cmp-nvim-lsp' },
			{ 'L3MON4D3/LuaSnip' },
		}
	},

	-- Aesthetics.
	{
		"folke/tokyonight.nvim",
		lazy = false,
		priority = 1000,
		opts = {},
		config = function()
			vim.cmd.colorscheme 'tokyonight'
		end,
	},
	{
		'nvim-lualine/lualine.nvim',
		opts = {
			options = {
				icons_enabled = false,
				theme = 'tokyonight',
				component_separators = '|',
				section_separators = '',
			},
		},
	},

	-- Miscellaneous.
	{ 'folke/which-key.nvim',  opts = {} },
	{ 'folke/neodev.nvim',     opts = {} },
	{ 'numToStr/Comment.nvim', opts = {} },
})

-- LSP autoconfiguration.
local lsp = require('lsp-zero').preset({})
lsp.on_attach(
	function(_, bufnr)
		lsp.default_keymaps({ buffer = bufnr })
	end
)
lsp.setup()

