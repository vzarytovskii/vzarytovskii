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
  local out = vim.fn.system({ 'git', 'clone', '--filter=blob:none', '--branch=stable', lazyrepo, lazypath })
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
    {
      'saghen/blink.cmp',
      version = '*',
    }
  },
  {
    defaults = { lazy = true },
  }
)

require('mason').setup()
require('mason-tool-installer').setup({
  ensure_installed = {
    'clangd',
  },
  auto_update = true,
  run_on_start = true,
})

require("nvim-treesitter.install").prefer_git = true
require('nvim-treesitter.configs').setup {
  ensure_installed = { 'c' },
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
}

require('blink.cmp').setup({
  signature = { enabled = true },
  completion = {
    menu = {
      auto_show = true,
      draw = {
        columns = {
          { "label", "label_description", gap = 1 },
          { "kind_icon", "kind" }
        },
      }
    },
    keyword = { range = 'full' },
    accept = { auto_brackets = { enabled = true }, },
    list = { selection = { preselect = true, auto_insert = true } },
    documentation = { auto_show = true },
    ghost_text = { enabled = true },
  },
  sources = { default = { 'lsp', 'path', 'buffer' } },
})

vim.diagnostic.config({
  virtual_text = { current_line = true }
})

--vim.api.nvim_create_autocmd('LspAttach', {
--  callback = function(ev)
--    local client = vim.lsp.get_client_by_id(ev.data.client_id)
--    if client:supports_method('textDocument/completion') then
--      vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = true })
--    end
--  end,
--})

vim.lsp.config('clangd', {
  cmd = { 'clangd', '--background-index' },
  root_markers = { '.clangd', 'compile_commands.json' },
  filetypes = { 'c', 'cpp' },
})

vim.lsp.enable { 'clangd' }

if vim.g.lsp_on_demands then
  vim.lsp.enable(vim.g.lsp_on_demands)
end
