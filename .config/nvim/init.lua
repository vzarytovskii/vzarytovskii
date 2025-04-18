vim.opt.completeopt = { 'fuzzy', 'menu', 'menuone', 'noselect', 'popup' }
vim.opt.foldcolumn = '1'
vim.opt.foldlevel = 99
vim.opt.foldlevelstart = 99
vim.opt.foldenable = true
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.exrc = true
vim.opt.smartindent = true
vim.opt.wrap = false
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.scrolloff = 8
vim.opt.signcolumn = "number"
vim.opt.isfname:append("@-@")

vim.wo.signcolumn = 'yes'
vim.wo.number = true
vim.wo.foldmethod = 'expr'
vim.wo.foldexpr = 'v:lua.vim.treesitter.foldexpr()'

local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'

if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = 'https://github.com/folke/lazy.nvim.git'
  local out = vim.fn.system({ 'git', 'clone', '--filter=blob:none', '--branch=main', lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { 'Failed to clone lazy.nvim:\n', 'ErrorMsg' },
      { out, 'WarningMsg' },
      { '\nPress any key to exit...' },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end

vim.opt.rtp:prepend(lazypath)

require('lazy').setup(
  {
    { 'williamboman/mason.nvim' },
    { 'WhoIsSethDaniel/mason-tool-installer.nvim' },
    { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
    { 'saghen/blink.cmp', version = '*', }
  },
  {
    install = { missing = true },
    checker = { enabled = true },
    defaults = { lazy = true },
  }
)

vim.diagnostic.config({
  virtual_text = { current_line = true }
})

local treesitter_configs = { 'c', 'cpp' }
local lsp_configs = {
  clangd = {
    cmd = { 'clangd', '--background-index' },
    root_markers = { '.clangd', 'compile_commands.json' },
    filetypes = { 'c', 'cpp' },
  }
}

require('mason').setup()
require('mason-tool-installer').setup({
  ensure_installed = vim.tbl_keys(lsp_configs),
  auto_update = true,
  run_on_start = true,
})

for name, config in pairs(lsp_configs) do
  vim.lsp.config(name, config)
end

vim.lsp.enable(vim.tbl_keys(lsp_configs))

if vim.g.lsp_on_demands then
  vim.lsp.enable(vim.g.lsp_on_demands)
end

require("nvim-treesitter.install").prefer_git = true
require('nvim-treesitter.configs').setup {
  ensure_installed = treesitter_configs,
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
   indent = {
    enable = true
  }
}

require('blink.cmp').setup({
  signature = {
    enabled = true,
  },
  completion = {
    trigger = {
      show_on_keyword = true,
      show_on_trigger_character = true,
    },
    menu = {
      auto_show = true,
      draw = {
        columns = {
          { "label", "label_description", gap = 1 },
          { "kind_icon", "kind" }
        },
        treesitter = { 'lsp' }
      }
    },
    keyword = {
      range = 'full'
    },
    accept = {
      auto_brackets = {
        enabled = true,
      },
    },
    list = {
      selection = {
        preselect = true,
        auto_insert = true,
      },
    },
    documentation = {
      auto_show = true,
    },
    ghost_text = {
      enabled = true
    },
  },
  sources = { default = { 'lsp', 'path', 'buffer' } },
})