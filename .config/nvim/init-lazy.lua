local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)
vim.g.mapleader = " "

require("lazy").setup({
  {
    'adelarsq/neofsharp.vim',
    config = function()
      vim.api.nvim_command('autocmd BufNewFile,BufRead *.fs,*.fsx,*.fsi,*.fsl,*.fsy set filetype=fsharp')
      vim.api.nvim_command('autocmd BufNewFile,BufRead *.fsproj,*.csproj,*.vbproj,*.cproj,*.proj set filetype=xml')
    end
  },
  {
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v2.x',
    dependencies = {
      -- LSP Support
      {'neovim/nvim-lspconfig'},
      {
        'williamboman/mason.nvim',
        build = function()
          pcall(vim.cmd, 'MasonUpdate')
        end,
      },
      { 'williamboman/mason-lspconfig.nvim' },
      { 'williamboman/mason-null-ls.nvim',
        event = { "BufReadPre", "BufNewFile" },
        dependencies = {
          "williamboman/mason.nvim",
          {
            "jose-elias-alvarez/null-ls.nvim",
            dependencies = {
              'nvim-lua/plenary.nvim'
            }
          },
        }
      },
      {'hrsh7th/nvim-cmp'},
      {'hrsh7th/cmp-nvim-lsp'},
    }
  },
  {
      'j-hui/fidget.nvim',
      event = 'LspAttach',
      branch = 'legacy',
      opts = { window = { blend = 0 } },
  },
})

require('mason').setup()
local lsp = require('lsp-zero').preset({})

lsp.ensure_installed({'fsautocomplete'})

lsp.on_attach(function(client, bufnr)
  lsp.default_keymaps({buffer = bufnr})
end)

lsp.setup()

require('null-ls').setup()
require('mason-null-ls').setup({
  ensure_installed = { 'fantomas' },
  automatic_installation = true,
  handlers = {}
})