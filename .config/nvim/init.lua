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

vim.opt.undofile = true
vim.opt.undodir = vim.fn.expand("~/.undodir")

vim.wo.signcolumn = 'yes'
vim.wo.number = true


vim.opt.termguicolors = true

vim.o.foldenable = true
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

local merge_table = function (t1, t2)
    for k,v in pairs(t2) do
        if type(v) == "table" then
            if type(t1[k] or false) == "table" then
                merge_table(t1[k] or {}, t2[k] or {})
            else
                t1[k] = v
            end
        else
            t1[k] = v
        end
    end
    return t1
end

vim.opt.rtp:prepend(lazypath)

require('lazy').setup(
  {
    {
      "vhyrro/luarocks.nvim",
      priority = 1000,
      config = true,
    },
    {
      "folke/tokyonight.nvim",
      lazy = false,
      priority = 1000,
      opts = {},
    },
    {
      "f-person/auto-dark-mode.nvim"
    },
    { 'mason-org/mason.nvim', build = ":MasonToolsUpdate", opts = {} },
    { 'WhoIsSethDaniel/mason-tool-installer.nvim' },
    { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
    {
        'MeanderingProgrammer/render-markdown.nvim',
        event = 'VeryLazy',
        ft = { 'markdown', 'octo' },
        dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.nvim' },
        opts = {},
    },
    { 'saghen/blink.cmp', version = '*', lazy=true, event='VeryLazy'},
    { 'j-hui/fidget.nvim' },
    {
      "y3owk1n/time-machine.nvim",
      opts = {
      }
    },
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
    { 'shortcuts/no-neck-pain.nvim', event = 'VeryLazy' }
  },
  {
    install = { missing = true },
    checker = { enabled = true },
    defaults = { lazy = true },
  }
)

vim.cmd[[colorscheme tokyonight]]

require('auto-dark-mode').setup({
  update_interval = 1000,
  set_dark_mode = function()
    vim.api.nvim_set_option("background", "dark")
    vim.cmd("colorscheme tokyonight")
  end,
  set_light_mode = function()
    vim.api.nvim_set_option("background", "light")
    vim.cmd("colorscheme tokyonight")
  end,
})

require('tokyonight').setup({})

vim.keymap.set('i', '<c-space>', vim.lsp.completion.get)

vim.diagnostic.config({
  virtual_text = { current_line = true },
  update_in_insert = true,
  underline = true,
  severity_sort = true,
})

local treesitter_configs = { 'c', 'cpp', 'rust', 'yaml', 'markdown', 'latex', 'html', 'typescript', 'javascript' }
local lsp_configs = {
  clangd = {
    cmd = { 'clangd', '--background-index', '--clang-tidy', '--all-scopes-completion', '--pch-storage=memory' },
    root_markers = { '.clangd', 'compile_commands.json', '.git', 'CMakeLists.txt' },
    filetypes = { 'c', 'cpp' },
    single_file_support = true,
  },
  ['typescript-language-server'] = {
    cmd = { 'typescript-language-server', '--stdio' },
    root_markers = { 'package.json', 'jsconfig.json' },
    filetypes = { 'javascript', 'javascriptreact', 'javascript.jsx', 'typescript', 'typescriptreact', 'typescript.tsx' },
    single_file_support = true,
  },
  ['rust-analyzer'] = {
    cmd = { "rust-analyzer" },
    filetypes = { "rust" },
    root_markers = {
        "Cargo.toml",
        "Cargo.lock",
        "rust-toolchain.toml",
        ".git",
        ".cargo"
    },
    settings = {
        ["rust-analyzer"] = {
            procMacro = { enable = true },
            cargo = { allFeatures = true },
            checkOnSave = true,
            check = {
                command = "clippy",
                extraArgs = { "--no-deps" },
            },
        },
    },
  },
  marksman = {
    cmd = { 'marksman' },
    root_markers = { '.marksman.toml', '.git', '*.md' },
    filetypes = { 'markdown', 'octo' },
    single_file_support = true,
  }
}
local tools = { 'clang-format', 'codelldb' }

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

vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client:supports_method('textDocument/foldingRange') then
      local win = vim.api.nvim_get_current_win()
      vim.wo[win][0].foldmethod = 'expr'
      vim.wo[win][0].foldexpr = 'v:lua.vim.lsp.foldexpr()'
    end
    if client:supports_method('textDocument/documentColor') then
      vim.lsp.document_color.enable(true, args.buf)
    end
  end,
 })
vim.api.nvim_create_autocmd('LspDetach', { command = 'setl foldexpr<' })
vim.api.nvim_create_autocmd("VimLeavePre", { callback = function() vim.iter(vim.lsp.get_clients()):each(function(client) client:stop() end) end, })


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
local all_tools = merge_table(vim.tbl_keys(lsp_configs), tools)
for _, tool in pairs(all_tools) do
    if vim.fn.executable(tool) == 0 then
        print("Installing tool: " .. tool)
        vim.cmd("MasonInstall " .. tool)
    end
end

--require('mason-tool-installer').setup({
--  ensure_installed = merge_table(vim.tbl_keys(lsp_configs), tools),
--  auto_update = true,
--  run_on_start = true,
--})


vim.lsp.enable(vim.tbl_keys(lsp_configs))

if vim.g.lsp_on_demands then
  vim.lsp.enable(vim.g.lsp_on_demands)
end


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
  appearance = {
    use_nvim_cmp_as_default = false,
    nerd_font_variant = "normal",
  },
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
      scrolloff = 1,
      scrollbar = false,
      border = nil,
      draw = {
        columns = {
            { "kind_icon" },
            { "label",      "label_description", gap = 1 },
            { "kind" },
            { "source_name" },
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
      window = {
        border = nil,
        scrollbar = false,
        winhighlight = 'Normal:BlinkCmpDoc,FloatBorder:BlinkCmpDocBorder,EndOfBuffer:BlinkCmpDoc',
      },
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

require"fidget".setup({})


require("time-machine").setup({})

require('octo').setup({})
require('neogit').setup({})

require("no-neck-pain").setup({})
