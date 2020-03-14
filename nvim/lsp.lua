-- ~/.config/nvim/lsp.lua

-- Disable unsolicited diagnostics.
function vim.lsp.util.buf_diagnostics_virtual_text() end

-- Load the NeoVim LSP collection.
local nvim_lsp = require('nvim_lsp')

-- Bash support.
nvim_lsp.bashls.setup{}

-- Python support (Microsoft).
-- nvim_lsp.pyls_ms.setup{}

-- Python support (Palantir).
nvim_lsp.pyls.setup{
	settings = {
		pyls = {
			plugins = {
				-- Linters.
				pycodestyle = {
					enabled = true,
					ignore = {'D107', 'E203', 'E501', 'W503'},
				},
				pydocstyle = {
					enabled = true,
					convention = 'pep257',
					addIgnore = {'D107'},
				},
				-- Fixers.
				black = {
					enabled = false,
				},
				isort = {
					enabled = false,
				},
			},
		}
	}
}

-- TeX support.
nvim_lsp.texlab.setup{}

-- Vim support.
nvim_lsp.vimls.setup{}
