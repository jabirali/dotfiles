-- Neovim configuration originally based on kickstart.nvim.

-- Bootstrap the package manager {{{
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

vim.opt.foldminlines = 0
vim.opt.conceallevel = 2

vim.opt.timeoutlen = 300
vim.opt.updatetime = 250

vim.opt.clipboard = "unnamedplus"
vim.opt.mouse = "a"
vim.opt.undofile = true
vim.opt.spelllang = "en_US,nb_NO"
vim.opt.tildeop = true

-- }}}

-- Package management (:help lazy.nvim) {{{
require("lazy").setup({
  -- General
  { "tpope/vim-sleuth" },
  { "numToStr/Comment.nvim", opts = {} },
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
  -- { "kevinhwang91/nvim-ufo",
  --  dependencies = {
  --    'kevinhwang91/promise-async'
  --  }
  --},

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
        mappings = {
          global = {
            org_agenda = '<leader>a',
            org_capture = '<leader>x',
          }
        }
      })
    end,
  },
  {
    "vimwiki/vimwiki",
    init = function()
      vim.g.vimwiki_list = { { path = "~/Sync/Wiki" } }
      vim.g.vimwiki_map_prefix = "<leader>e"
    end,
    config = function()
      vim.keymap.set("n", "<leader>i", "<cmd>VimwikiIndex<cr>")
      vim.keymap.set("n", "<leader>j", "<cmd>VimwikiMakeDiaryNote<cr>")
    end,
  },
  { "tools-life/taskwiki" },
  { "jbyuki/nabla.nvim" },

  -- Aesthetics
  {
    "folke/tokyonight.nvim",
    priority = 1000,
    init = function()
      vim.cmd.colorscheme("tokyonight-night")
      --vim.cmd.hi {"link Folded Comment", bang=true}
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


--require('ufo').setup()
-- }}}

-- Keybindings {{{
--- Miscellaneous
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

--- Keybinds to make split navigation easier.
vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Focus left" })
vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Focus right" })
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Focus lower" })
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Focus upper" })

--- CUA-like keybindings for common actions
vim.keymap.set("n", "<leader>n", "<cmd>new<cr>", { desc = "New buffer" })
vim.keymap.set("n", "<leader>s", "<cmd>write<cr>", { desc = "Save buffer" })
vim.keymap.set("n", "<leader>t", "<cmd>tabnew<cr>", { desc = "New tab" })
vim.keymap.set("n", "<leader>w", "<cmd>close<cr>", { desc = "Close window or tab" })
vim.keymap.set("n", "<leader>q", "<cmd>quit<cr>", { desc = "Close window or vim" })

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
vim.keymap.set("n", "<leader>fh", builtin.help_tags)
vim.keymap.set("n", "<leader>ff", builtin.find_files)
vim.keymap.set("n", "<leader>fg", builtin.live_grep)
vim.keymap.set("n", "<leader>fr", builtin.oldfiles)
--vim.keymap.set("n", "<leader><leader>", builtin.buffers)
vim.keymap.set("n", "<leader>fn", function() builtin.find_files({ cwd = vim.fn.stdpath("config") }) end)

-- }}}

-- vim: fdm=marker ts=2 sw=2 et
