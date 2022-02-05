local fn = vim.fn

local ok, packer = pcall(require, "packer")
if not ok then
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  print "Cloning packer..."
  fn.delete(packer_path, "rf")
  
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
  end

  vim.cmd [[packadd packer.nvim]]

  packer_exists, packer = pcall(require, "packer")

  if packer_exists then
    print "Packer cloned successfully."
  else
    error("Couldn't clone packer !\nPacker path: " .. packer_path .. "\n" .. packer)
  end
end

packer.startup({function()
  
  -- General
  use 'wbthomason/packer.nvim'
  use 'lewis6991/impatient.nvim'

  -- UI
  use { 'hoob3rt/lualine.nvim', requires = { 'kyazdani42/nvim-web-devicons', opt = true } }
  use { 'marko-cerovac/material.nvim' }

  -- Dev: Autocomplete, TreeSitter, LSP, etc.
  use 'adelarsq/neofsharp.vim'

  use 'neovim/nvim-lspconfig'
  use 'williamboman/nvim-lsp-installer'
 
  use 'glepnir/lspsaga.nvim'

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

  if packer_bootstrap then
    require('packer').sync()
  end
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
