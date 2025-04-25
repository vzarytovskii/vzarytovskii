vim.opt.completeopt = { 'fuzzy', 'menu', 'menuone', 'noselect', 'popup' }
vim.opt.foldlevel = 99
vim.opt.foldlevelstart = 99
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

vim.opt.foldenable = true
vim.o.foldexpr = "v:lua.vim.treesitter.foldexpr()"
vim.o.foldcolumn = "1"
vim.o.foldlevel = 99
vim.o.foldlevelstart = 99
vim.o.foldenable = true

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
    {
        'MeanderingProgrammer/render-markdown.nvim',
        ft = { 'markdown', 'octo' },
        dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.nvim' }, -- if you use the mini.nvim suite
        ---@module 'render-markdown'
        ---@type render.md.UserConfig
        opts = {},
    },
    { 'saghen/blink.cmp', version = '*', },
    {
      'pwntester/octo.nvim',
      cmd = { 'Octo' },
      event = 'VeryLazy',
      dependencies = {
        'nvim-lua/plenary.nvim',
        'nvim-telescope/telescope.nvim',
        'nvim-tree/nvim-web-devicons',
      },
    },
    {
      "NeogitOrg/neogit",
      event = 'VeryLazy',
      dependencies = {
        "nvim-lua/plenary.nvim",
        "sindrets/diffview.nvim",
        "nvim-telescope/telescope.nvim",
      },
    },
    { 'chrisgrieser/nvim-origami', event = 'VeryLazy', opts = {} },
    { 'shortcuts/no-neck-pain.nvim' }
  },
  {
    install = { missing = true },
    checker = { enabled = true },
    defaults = { lazy = true },
  }
)

vim.keymap.set('i', '<c-space>', vim.lsp.completion.get)

vim.diagnostic.config({
  virtual_text = { current_line = true }
})

local treesitter_configs = { 'c', 'cpp', 'markdown', 'latex', 'html' }
local lsp_configs = {
  clangd = {
    cmd = { 'clangd', '--background-index' },
    root_markers = { '.clangd', 'compile_commands.json' },
    filetypes = { 'c', 'cpp' },
  }
}

vim.lsp.config('*', {
  capabilities = {
    textDocument = {
      semanticTokens = {
        multilineTokenSupport = true,
      }
    },
    foldingRange = {
        dynamicRegistration = false,
        lineFoldingOnly = true,
    }
  },
  root_markers = { '.git' },
})

for name, config in pairs(lsp_configs) do
  vim.lsp.config(name, config)
end

vim.lsp.enable(vim.tbl_keys(lsp_configs))

if vim.g.lsp_on_demands then
  vim.lsp.enable(vim.g.lsp_on_demands)
end

vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('lsp-attach', { clear = true }),
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client:supports_method('textDocument/foldingRange') then
      local win = vim.api.nvim_get_current_win()
      vim.wo[win][0].foldmethod = 'expr'
      vim.wo[win][0].foldexpr = 'v:lua.vim.lsp.foldexpr()'
    end
  end,
 })

vim.api.nvim_create_autocmd('LspDetach', { command = 'setl foldexpr<' })

vim.treesitter.language.register('markdown', 'octo')

require('nvim-treesitter.install').prefer_git = true
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

require('mason').setup()
require('mason-tool-installer').setup({
  ensure_installed = vim.tbl_keys(lsp_configs),
  auto_update = true,
  run_on_start = true,
})

require('render-markdown').setup({
  enabled = true,
  file_types = { "markdown", "octo" },
  completions = { blink = { enabled = true } },
  render_modes = true,
  injections = {
      gitcommit = {
          enabled = true,
          query = [[
              ((message) @injection.content
                  (#set! injection.combined)
                  (#set! injection.include-children)
                  (#set! injection.language "markdown"))
          ]],
      },
  },
})

require('blink.cmp').setup({
  keymap = {
    ['<CR>'] = { 'accept', 'fallback' },
    ['<Tab>'] = { 'accept', 'fallback' },
    ['<Right>'] = { 'accept', 'fallback' },
    ['<Esc>'] = { 'hide', 'fallback' },
  },
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
  fuzzy = {
    sorts = {
      'exact',
      -- defaults
      'score',
      'sort_text',
    },
  }
})

require('octo').setup({})
require('neogit').setup({})

require("no-neck-pain").setup({})
