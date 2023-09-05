-- ~/.config/nvim/init.lua
-- vim: foldmethod=marker

-- Core settings {{{
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
vim.o.autoread = true
vim.o.breakindent = true
vim.o.clipboard = 'unnamedplus'
vim.o.laststatus = 3
vim.o.linebreak = true
vim.o.shiftwidth = 4
vim.o.showcmd = false
vim.o.showmode = false
vim.o.signcolumn = false
vim.o.softtabstop = -1
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.tabstop = 4
vim.o.tildeop = true
vim.o.virtualedit = 'block'
vim.o.wrap = false
-- }}}

-- Bootstrap package manager {{{
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
-- }}}

-- Handle package management {{{
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
				component_separators = { left = '', right = ''},
				section_separators = { left = '', right = ''},
			},
		},
	},

	-- Miscellaneous.
	{ 'folke/which-key.nvim',  opts = {} },
	{ 'folke/neodev.nvim',     opts = {} },
	{ 'numToStr/Comment.nvim', opts = {} },
})
-- }}}

-- LSP autoconfiguration {{{
local lsp = require('lsp-zero').preset({})
lsp.on_attach(
function(_, bufnr)
	lsp.default_keymaps({ buffer = bufnr })
end
)
lsp.setup()
-- }}}
