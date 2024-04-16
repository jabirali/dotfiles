-- Neovim configuration originally based on kickstart.nvim.

-- Bootstrap the package manager (:help lazy.nvim) {{{
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	local lazyrepo = "https://github.com/folke/lazy.nvim.git"
	vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
end
vim.opt.rtp:prepend(lazypath)
-- }}}

-- Vim options (:help option-list) {{{
vim.g.mapleader = " "
vim.g.maplocalleader = ","

vim.opt.laststatus = 3
vim.opt.relativenumber = true
vim.opt.scrolloff = 10
vim.opt.showmode = false
vim.opt.showcmd = false
vim.opt.signcolumn = "no"

vim.opt.expandtab = true
vim.opt.shiftwidth = 0
vim.opt.smarttab = true
vim.opt.tabstop = 4

vim.opt.wrap = false
vim.opt.breakindent = true
vim.opt.breakindentopt = "list:-1"
vim.opt.linebreak = true
vim.opt.virtualedit = 'block'

vim.opt.splitbelow = true
vim.opt.splitright = true

vim.opt.hlsearch = true
vim.opt.ignorecase = true
vim.opt.inccommand = "split"
vim.opt.smartcase = true

vim.opt.timeoutlen = 300
vim.opt.updatetime = 250

vim.opt.clipboard = "unnamedplus"
vim.opt.mouse = "a"
vim.opt.undofile = true
vim.opt.spelllang = "en_US,nb_NO"
vim.opt.tildeop = true

-- }}}

-- Package management {{{
require("lazy").setup({
	-- General
	{ "tpope/vim-sleuth" },
	{ "numToStr/Comment.nvim", opts = {} },
	{ "folke/which-key.nvim", opts = {} },
	{
		"echasnovski/mini.nvim",
		config = function()
			require("mini.ai").setup({ n_lines = 500 })
			require("mini.surround").setup()
			require("mini.statusline").setup({ use_icons = true })
		end,
	},

	-- LSP support
	{ "VonHeikemen/lsp-zero.nvim", branch = "v3.x" },
	{ "neovim/nvim-lspconfig" },
	{ "hrsh7th/cmp-nvim-lsp" },
	{ "hrsh7th/nvim-cmp" },
	{ "L3MON4D3/LuaSnip" },

	-- TreeSitter support
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		opts = {
			ensure_installed = { "python", "lua", "luadoc", "markdown", "vim", "vimdoc" },
			auto_install = true,
			highlight = { enable = true },
			indent = { enable = true },
		},
		config = function(_, opts)
			require("nvim-treesitter.configs").setup(opts)
		end,
	},

	-- Fuzzy finder
	{
		"nvim-telescope/telescope.nvim",
		event = "VimEnter",
		branch = "0.1.x",
		dependencies = {
			{ "nvim-lua/plenary.nvim" },
			{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
			{ "nvim-telescope/telescope-ui-select.nvim" },
			{ "nvim-tree/nvim-web-devicons" },
		},
		opts = {},
	},

	-- Note taking
	{
		'nvim-orgmode/orgmode',
		event = 'VeryLazy',
		ft = { 'org' },
		config = function()
			require('orgmode').setup({
				org_agenda_files = '~/Sync/Org/**/*',
				org_default_notes_file = '~/Sync/Org/inbox.org',
				org_hide_leading_stars = true
			})
		end,
	},
	{
		"vimwiki/vimwiki",
		init = function()
			vim.g.vimwiki_list = { { path = "~/Sync/Wiki" } }
			vim.g.vimwiki_map_prefix = "<leader>e"
		end,
	},
	{ "tools-life/taskwiki" },
	{ "jbyuki/nabla.nvim" },
	{ 'anuvyklack/pretty-fold.nvim',
		config = function()
			require('pretty-fold').setup()
		end
	},

	-- Aesthetics
	{
		"folke/tokyonight.nvim",
		priority = 1000,
		init = function()
			vim.cmd.colorscheme("tokyonight-night")
			vim.cmd.hi("Comment gui=none")
		end,
	},
})

local lsp_zero = require('lsp-zero')
lsp_zero.on_attach(function(client, bufnr)
  -- see :help lsp-zero-keybindings
  -- to learn the available actions
  lsp_zero.default_keymaps({buffer = bufnr})
end)
require('lspconfig').pyright.setup({})
-- }}}

-- Keybindings {{{

-- Document existing key chains
-- require("which-key").register({
-- 	["<leader>c"] = { name = "[C]ode", _ = "which_key_ignore" },
-- 	["<leader>d"] = { name = "[D]ocument", _ = "which_key_ignore" },
-- 	["<leader>r"] = { name = "[R]ename", _ = "which_key_ignore" },
-- 	["<leader>s"] = { name = "[F]ind", _ = "which_key_ignore" },
-- 	["<leader>w"] = { name = "[W]orkspace", _ = "which_key_ignore" },
-- })

vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- Diagnostic keymaps
-- vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Go to previous [D]iagnostic message" })
-- vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Go to next [D]iagnostic message" })
-- vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, { desc = "Show diagnostic [E]rror messages" })
-- vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })

-- Exit terminal mode in the builtin terminal with a shortcut that is a bit easier
-- for people to discover. Otherwise, you normally need to press <C-\><C-n>, which
-- is not what someone will guess without a bit more experience.
--
-- NOTE: This won't work in all terminal emulators/tmux/etc. Try your own mapping
-- or just use <C-\><C-n> to exit terminal mode
vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

-- TIP: Disable arrow keys in normal mode
-- vim.keymap.set('n', '<left>', '<cmd>echo "Use h to move!!"<CR>')
-- vim.keymap.set('n', '<right>', '<cmd>echo "Use l to move!!"<CR>')
-- vim.keymap.set('n', '<up>', '<cmd>echo "Use k to move!!"<CR>')
-- vim.keymap.set('n', '<down>', '<cmd>echo "Use j to move!!"<CR>')

-- Keybinds to make split navigation easier.
vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Focus left" })
vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Focus right" })
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Focus lower" })
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Focus upper" })

-- CUA-like common actions
vim.keymap.set("n", "<leader>n", "<cmd>new<cr>", { desc = "New buffer" })
vim.keymap.set("n", "<leader>s", "<cmd>write<cr>", { desc = "Save buffer" })
vim.keymap.set("n", "<leader>t", "<cmd>tabnew<cr>", { desc = "New tab" })
vim.keymap.set("n", "<leader>w", "<cmd>close<cr>", { desc = "Close window or tab" })
vim.keymap.set("n", "<leader>1", "1gt", { desc = "Tab 1" })
vim.keymap.set("n", "<leader>2", "2gt", { desc = "Tab 2" })
vim.keymap.set("n", "<leader>3", "3gt", { desc = "Tab 3" })
vim.keymap.set("n", "<leader>4", "4gt", { desc = "Tab 4" })
vim.keymap.set("n", "<leader>5", "5gt", { desc = "Tab 5" })
vim.keymap.set("n", "<leader>6", "6gt", { desc = "Tab 6" })
vim.keymap.set("n", "<leader>7", "7gt", { desc = "Tab 7" })
vim.keymap.set("n", "<leader>8", "8gt", { desc = "Tab 8" })
vim.keymap.set("n", "<leader>9", "9gt", { desc = "Tab 9" })

-- Telescope
local builtin = require("telescope.builtin")
vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "[F]ind [H]elp" })
vim.keymap.set("n", "<leader>fk", builtin.keymaps, { desc = "[F]ind [K]eymaps" })
vim.keymap.set("n", "<leader>ff", builtin.find_files, { desc = "[F]ind [F]iles" })
vim.keymap.set("n", "<leader>fs", builtin.builtin, { desc = "[F]ind [S]elect Telescope" })
vim.keymap.set("n", "<leader>fw", builtin.grep_string, { desc = "[F]ind current [W]ord" })
vim.keymap.set("n", "<leader>fg", builtin.live_grep, { desc = "[F]ind by [G]rep" })
vim.keymap.set("n", "<leader>fd", builtin.diagnostics, { desc = "[F]ind [D]iagnostics" })
vim.keymap.set("n", "<leader>fr", builtin.resume, { desc = "[F]ind [R]esume" })
vim.keymap.set("n", "<leader>f.", builtin.oldfiles, { desc = '[F]ind Recent Files ("." for repeat)' })
-- vim.keymap.set("n", "<leader><leader>", builtin.buffers, { desc = "[ ] Find existing buffers" })
vim.keymap.set("n", "<leader>fn", function()
	builtin.find_files({ cwd = vim.fn.stdpath("config") })
end, { desc = "[F]ind [N]eovim files" })

-- }}}

-- vim: fdm=marker
