-- ~/.config/nvim/lua/boot.lua:
-- The first time you invoke `nvim` on a new computer, this file autoinstalls
-- the package manager `packer.nvim`. This is required for `pack.lua` to load.

local packer_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
if vim.fn.empty(vim.fn.glob(packer_path)) > 0 then
	vim.api.nvim_command('!git clone https://github.com/wbthomason/packer.nvim ' .. packer_path)
end
