-- ~/.config/nvim/init.lua
-- vim: foldmethod=marker foldmarker=▼,▲

--▼ Short-hand notation
local g = vim.g
local o = vim.o
local map = vim.keymap.set
--▲

--▼ Core settings
g.mapleader = ' '
g.maplocalleader = ','
g.netrw_banner = false

o.autoread = true
o.breakindent = true
o.clipboard = 'unnamedplus'
o.laststatus = 3
o.linebreak = true
o.shiftwidth = 4
o.showcmd = false
o.showmode = false
o.softtabstop = -1
o.splitbelow = true
o.splitright = true
o.tabstop = 4
o.tildeop = true
o.virtualedit = 'block'
o.wrap = false
o.signcolumn = 'no'
--▲

--▼ Package manager
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
--▲

--▼ Package management
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
		},
		config = function()
			-- Enable keybindings in LSP-supported buffers.
			local lsp = require('lsp-zero').preset('recommended')
			lsp.on_attach(function(_, bufnr)
				lsp.default_keymaps({ buffer = bufnr })
			end)

			-- Custom configuration of specific LSP servers.
			local conf = require('lspconfig')
			conf.lua_ls.setup(lsp.nvim_lua_ls())

			-- Execute the LSP autoconfiguration.
			lsp.setup()
		end
	},

	-- Interface.
	{
		'nvim-telescope/telescope.nvim',
		branch = '0.1.x',
		dependencies = {
			'nvim-lua/plenary.nvim'
		},
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
				component_separators = { left = '', right = '' },
				section_separators = { left = '', right = '' },
			},
		},
	},

	-- Language support.
	{
		"lervag/vimtex",
		lazy = false,
		init = function()
			vim.g.vimtex_mappings_prefix = '<localleader>'
			vim.g.vimtex_quickfix_open_on_warning = 'false'
			vim.g.vimtex_view_method = 'skim'
		end,
	},

	-- Miscellaneous.
	{ 'folke/which-key.nvim',  opts = {} },
	{ 'numToStr/Comment.nvim', opts = {} },
})
--▲

--▼ Keybindings
map('', ';', ':')
map('n', '<tab>', 'za')
map('n', '<S-tab>', 'zM')
map('v', '<tab>', '>gv')
map('v', '<S-tab>', '<gv')

map('n', '<leader><cr>', '<cmd>split | term<cr>')
--▲

--▼ GUI configuration
o.guifont = 'JetBrains Mono:h14'

map('', '<D-s>', '<cmd>write<cr>')
map('', '<D-w>', '<cmd>close<cr>')
map('', '<D-q>', '<cmd>quit<cr>')
map('', '<D-d>', '<cmd>split<cr>')
map('', '<D-t>', '<cmd>tabnew<cr>')
map('', '<D-]>', '<cmd>tabnext<cr>')
map('', '<D-[>', '<cmd>tabprev<cr>')
--▲
