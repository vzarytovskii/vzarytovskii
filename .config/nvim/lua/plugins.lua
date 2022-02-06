local fn = vim.fn

local ok, packer = pcall(require, "packer")

if not ok then
  local packer_bootstrap = false
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

  print "Cloning packer..."
  fn.delete(install_path, "rf")

  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    packer_bootstrap = true
  end

  vim.cmd [[packadd packer.nvim]]

  ok, packer = pcall(require, "packer")

  if ok then
    print "Packer cloned successfully."
  else
    error("Couldn't clone packer !\nPacker path: " .. install_path .. "\n" .. packer)
  end

  if packer_bootstrap then
    packer.sync()
  end

end

vim.opt.list = true
vim.opt.listchars:append("space:⋅")
vim.opt.listchars:append("eol:↴")
vim.wo.signcolumn = "yes"
vim.wo.number = true

packer.startup({function(use)

  -- General
  use 'wbthomason/packer.nvim'
  use 'lewis6991/impatient.nvim'

  use 'nvim-telescope/telescope.nvim'
  -- UI
  use { 'hoob3rt/lualine.nvim', requires = { 'kyazdani42/nvim-web-devicons', opt = true } }
  use { 'marko-cerovac/material.nvim' }
  use 'lukas-reineke/indent-blankline.nvim'

  -- Dev: Autocomplete, TreeSitter, LSP, etc.
  use 'adelarsq/neofsharp.vim'

  use 'neovim/nvim-lspconfig'
  use 'williamboman/nvim-lsp-installer'

  use 'jose-elias-alvarez/null-ls.nvim'

  use 'tami5/lspsaga.nvim'

  use 'hrsh7th/nvim-cmp'
  use 'hrsh7th/cmp-nvim-lsp'

  use 'saadparwaiz1/cmp_luasnip'
  use 'L3MON4D3/LuaSnip'

  use { 'nvim-treesitter/nvim-treesitter', config = 'vim.cmd [[TSUpdate]]' }

  use { "folke/trouble.nvim", requires = "kyazdani42/nvim-web-devicons" }

  -- DAP
  use 'mfussenegger/nvim-dap'
  use { "rcarriga/nvim-dap-ui", requires = {"mfussenegger/nvim-dap"} }
  use 'theHamsta/nvim-dap-virtual-text'
  use "Pocco81/DAPInstall.nvim"

  -- Git
  use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } }
  use { 'pwntester/octo.nvim', requires = { 'nvim-lua/plenary.nvim', 'nvim-telescope/telescope.nvim', 'kyazdani42/nvim-web-devicons' } }

end,
config = {
  auto_clean = true,
  auto_reload_compiled = true,
  ensure_dependencies = true,
  compile_on_sync = true,
  compile_path = vim.fn.stdpath('config')..'/lua/packer_compiled.lua',
  display = {
    open_fn = function()
      return require('packer.util').float({ border = 'single' })
    end
  }
}})

vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])

require 'ui'
require 'dev'
