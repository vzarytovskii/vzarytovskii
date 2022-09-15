local ok, impatient = pcall(require, 'impatient')
if ok then
  impatient.enable_profile()
end

local fn = vim.fn

local ok, packer = pcall(require, "packer")
local packer_bootstrap = false

if not ok then

  if fn.input("Packer seems to be missing. Download? (y for yes): ") ~= "y" then
    return
  end

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
    print "Packer loaded successfully."
  else
    error("Couldn't load packer !\nPacker path: " .. install_path .. "\n" .. packer)
  end

end

vim.opt.list = true
vim.opt.listchars:append("space:⋅")
vim.opt.listchars:append("eol:↴")
vim.opt.updatetime = 250
--vim.opt.completeopt:append({'menuone','noselect','noinsert'})
--vim.opt.completeopt:remove('preview')
vim.opt.shortmess:append('c')
vim.wo.signcolumn = "yes"
vim.wo.number = true

packer.startup({function(use)

  -- General
  use 'wbthomason/packer.nvim'
  use 'lewis6991/impatient.nvim'

  use 'nvim-telescope/telescope.nvim'
  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build', requires = { 'nvim-telescope/telescope.nvim' } }

  -- UI
  use { 'hoob3rt/lualine.nvim', requires = { 'kyazdani42/nvim-web-devicons', opt = true } }
  use 'folke/tokyonight.nvim'

  -- Dev: Autocomplete, TreeSitter, LSP, etc.
  use 'adelarsq/neofsharp.vim'

  use {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      'WhoIsSethDaniel/mason-tool-installer.nvim',
      "neovim/nvim-lspconfig",
  }

  use 'jose-elias-alvarez/null-ls.nvim'
  use { 'glepnir/lspsaga.nvim', branch = "main"}
  use 'ray-x/lsp_signature.nvim'
  use 'jubnzv/virtual-types.nvim'
  use 'onsails/lspkind-nvim'
  use 'j-hui/fidget.nvim'
  use 'github/copilot.vim'

  use {
    'hrsh7th/nvim-cmp',
    requires = {
      'hrsh7th/cmp-nvim-lsp',
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      'saadparwaiz1/cmp_luasnip',
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "hrsh7th/cmp-nvim-lua",
      "hrsh7th/cmp-copilot"
    }
  }
  use {
    'L3MON4D3/LuaSnip',
    requires = {
      "rafamadriz/friendly-snippets"
    }
  }

  use {
    'nvim-treesitter/nvim-treesitter',
    requires = {
      'nvim-treesitter/playground'
    },
    config = 'vim.cmd [[TSUpdate]]' 
  }

  use "smjonas/inc-rename.nvim"

  use { "folke/trouble.nvim", requires = "kyazdani42/nvim-web-devicons" }

  -- Comments
  use 'numToStr/Comment.nvim'

  -- DAP
  use 'mfussenegger/nvim-dap'
  use { "rcarriga/nvim-dap-ui", requires = {"mfussenegger/nvim-dap"} }
  use 'theHamsta/nvim-dap-virtual-text'
  use "Pocco81/DAPInstall.nvim"

  -- Git
  use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } }
  use { 'pwntester/octo.nvim', requires = { 'nvim-lua/plenary.nvim', 'nvim-telescope/telescope.nvim', 'kyazdani42/nvim-web-devicons' } }
  use { 'TimUntersberger/neogit', requires = 'nvim-lua/plenary.nvim' }
  use {
    'ldelossa/gh.nvim',
    requires = { { 'ldelossa/litee.nvim' } }
  }

  use {
    'ruifm/gitlinker.nvim',
    requires = 'nvim-lua/plenary.nvim',
  }
  -- Testing
  use {
    "nvim-neotest/neotest",
    requires = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
      "antoinemadec/FixCursorHold.nvim"
    }
  }
  -- Misc
  use{ 'anuvyklack/pretty-fold.nvim',
    requires = 'anuvyklack/keymap-amend.nvim', -- only for preview
  }
  use { 'anuvyklack/fold-preview.nvim',
   requires = 'anuvyklack/keymap-amend.nvim'
  }
  use { 'danymat/neogen', requires = { 'nvim-treesitter/nvim-treesitter' } }
  use "b0o/schemastore.nvim"
  use "folke/which-key.nvim"
  use {
    'esensar/nvim-dev-container',
    requires = { 'nvim-treesitter/nvim-treesitter' }
  }
  use { 'antoinemadec/FixCursorHold.nvim' }
  use { 'kensyo/nvim-scrlbkun' }
  use 'karb94/neoscroll.nvim'

end,
config = {
  auto_clean = true,
  auto_reload_compiled = true,
  ensure_dependencies = true,
  compile_on_sync = true,
  compile_path = vim.fn.stdpath('config')..'/packer_compiled.lua',
  display = {
    open_fn = function()
      return require('packer.util').float({ border = 'single' })
    end
  }
}})

if packer_bootstrap then
  packer.sync()
  vim.cmd 'autocmd User PackerComplete ++once lua require("packer").compile()'
  --packer.compile()
  --vim.api.nvim_command "PackerCompile"
  return
end

local packer_group = vim.api.nvim_create_augroup('Packer', { clear = true })
vim.api.nvim_create_autocmd('BufWritePost', {
  command = 'source <afile> | PackerCompile',
  group = packer_group,
  pattern = vim.fn.expand '$MYVIMRC',
})

local function merge(t1, t2)
    for k, v in pairs(t2) do
        if (type(v) == "table") and (type(t1[k] or false) == "table") then
            merge(t1[k], t2[k])
        else
            t1[k] = v
        end
    end
    return t1
end

local function get_keys(t)
  local keys={}
  for key,_ in pairs(t) do
    table.insert(keys, key)
  end
  return keys
end

local function set_keymap(...) vim.api.nvim_set_keymap(...) end

vim.g.cursorhold_updatetime = 100

vim.g.tokyonight_style = "night"
vim.cmd[[colorscheme tokyonight]]

require('lualine').setup {
  options = {
    theme = 'tokyonight'
  }
}
local telescope = require('telescope')
local telescope_actions = require('telescope.actions')
local telescope_builtin = require('telescope.builtin')
telescope.setup {
  defaults = {
    file_sorter = require("telescope.sorters").get_fuzzy_file,
    file_ignore_patterns = { "node_modules" },
    generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
    vimgrep_arguments = {
      "rg",
      "--color=never",
      "--no-heading",
      "--with-filename",
      "--line-number",
      "--column",
      "--smart-case",
      "--trim" -- add this value
    },
    layout_config = {
      vertical = { width = 0.5 }
    },
    mappings = {
      i = {
        ['<C-u>'] = false,
        ['<C-d>'] = false,
        ["<esc>"] = telescope_actions.close,
        ["<C-g>"] = telescope_actions.close
      }
    },
  },
  pickers = {
    find_files = {
      --theme = "dropdown",
      --find_command = { 'rg', '--files' },
      find_command = { "fdfind", "--type", "f", "--strip-cwd-prefix" },
    }
  },
  extensions = {
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    }
  }
}

project_files = function()
  local opts = {} -- define here if you want to define something
  local ok = pcall(telescope_builtin.git_files, opts)
  if not ok then
    telescope_builtin.find_files(opts)
  end
end

set_keymap('n', '<leader>/', ':Telescope current_buffer_fuzzy_find<CR>', { noremap = true, silent= true })
set_keymap('n', '<C-s>', ':Telescope current_buffer_fuzzy_find<CR>', { noremap = true, silent= true })
set_keymap('n', '<C-f>', '<CMD>lua project_files()<CR>', { noremap = true, silent= true })
set_keymap('n', '<M-s>', ':Telescope live_grep<CR>', { noremap = true, silent= true })
set_keymap('n', '<C-b>', ':Telescope buffers<CR>', { noremap = true, silent= true })

pcall(require('telescope').load_extension, 'fzf')

vim.api.nvim_command('autocmd BufNewFile,BufRead *.fs,*.fsx,*.fsi,*.fsl,*.fsy set filetype=fsharp')

vim.opt.tabstop=8
vim.opt.shiftwidth=2
vim.opt.expandtab=true
vim.opt.smartindent=true
vim.opt.hlsearch=true

local ok, treesitter = pcall(require, "nvim-treesitter.configs")

if not ok then
  return
end

treesitter.setup {
  ensure_installed = "all",
  sync_install = false,
  highlight = {
    enable = false,
  },
  indent = {
    enable = true
  },
  context_commentstring = {
    enable = true,
    enable_autocmd = false,
  },
  autopairs = {
    enable = true,
  },
  rainbow = {
    enable = true,
    disable = { "html" },
    extended_mode = false,
    max_file_lines = nil,
  },
}

require('gitsigns').setup()
require('octo').setup()
require('neogit').setup()
require"gitlinker".setup()

require('litee.lib').setup()
require('litee.gh').setup()

set_keymap('n', '<leader>gs', ':Neogit<CR>', { noremap = true, silent= true })

local capabilities = vim.lsp.protocol.make_client_capabilities()

capabilities.textDocument.completion.completionItem = {
  documentationFormat = { "markdown", "plaintext" },
  snippetSupport = true,
  preselectSupport = true,
  insertReplaceSupport = true,
  labelDetailsSupport = true,
  deprecatedSupport = true,
  commitCharactersSupport = true,
  tagSupport = { valueSet = { 1 } },
  resolveSupport = {
    properties = {
      "documentation",
      "detail",
      "additionalTextEdits",
    },
  },
}

capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

local virtualtypes = require('virtualtypes')

local on_attach = function(client, bufnr)
    if client.supports_method("textDocument/codeLens") then
      virtualtypes.on_attach(client, bufnr)
    end

    client.server_capabilities.documentFormattingProvider = false
    client.server_capabilities.documentRangeFormattingProvider = false
--  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

--  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
end

local tools = {
  -- Formatters, linters and DAPs:
  'shellcheck',
  'prettier',
  --'codespell'
  --'fantomas'
}

local runtime_path = vim.split(package.path, ';')
table.insert(runtime_path, 'lua/?.lua')
table.insert(runtime_path, 'lua/?/init.lua')

local servers = {
  diagnosticls = {},
  jsonls = {
    settings = {
      json = {
        schemas = require('schemastore').json.schemas(),
        validate = { enable = true },
      },
    },
  },
  grammarly = {},
  marksman = {},
  sumneko_lua = {
    settings = {
      Lua = {
        runtime = {
          version = 'LuaJIT',
          path = runtime_path,
        },
        diagnostics = {
          globals = { 'vim' },
        },
        workspace = { library = vim.api.nvim_get_runtime_file('', true) },
        telemetry = { enable = false, },
      },
    },
  },
  fsautocomplete = {
    cmd = { "fsautocomplete" },
    filetypes = { "fsharp" },
    init_options = {
       AutomaticWorkspaceInit = true
    },
  },
  omnisharp = { use_mono = false }
}

local server_names = get_keys(servers)

require("mason").setup()
local mason_lspconfig = require("mason-lspconfig")
local nvim_lsp = require('lspconfig')

mason_lspconfig.setup({
  ensure_installed = server_names,
  automatic_installation = true
})

mason_lspconfig.setup_handlers {
    function (server_name)
        local server_opts = servers[server_name] or {}
        local opts = {
          on_attach = on_attach,
          capabilities = capabilities,
          flags = {
            debounce_text_changes = 150,
          }
        }

        merge(opts, server_opts)

        nvim_lsp[server_name].setup(opts)
    end,
    --["rust_analyzer"] = function ()
    --    require("rust-tools").setup {}
    --end
}

require('mason-tool-installer').setup {
  ensure_installed = tools,
  auto_update = false,
  run_on_start = true
}

require"fidget".setup{}

local null_ls = require("null-ls")
null_ls.setup({
    sources = {
        null_ls.builtins.completion.spell,
        null_ls.builtins.code_actions.gitsigns,
        --null_ls.builtins.code_actions.refactoring,
        null_ls.builtins.completion.luasnip,
        --null_ls.builtins.completion.vsnip,
        --null_ls.builtins.diagnostics.actionlint,
        --null_ls.builtins.diagnostics.checkmake,
        --null_ls.builtins.diagnostics.codespell,
        -- null_ls.builtins.diagnostics.cspell,
        --null_ls.builtins.diagnostics.editorconfig_checker,
        --null_ls.builtins.diagnostics.luacheck,
        --null_ls.builtins.diagnostics.markdownlint,
        --null_ls.builtins.code_actions.shellcheck
    },
})

require('lsp_signature').setup({
        bind = true,
        floating_window = true,
        hint_enable = false,
})

require('lspsaga').init_lsp_saga({
  code_action_lightbulb = {
    enable = true,
    sign = true,
    enable_in_insert = true,
    sign_priority = 20,
    virtual_text = false,
  },
})

vim.cmd [[autocmd CursorHold,CursorHoldI * :Lspsaga show_cursor_diagnostics ]]
--vim.cmd [[autocmd CursorHold,CursorHoldI * :Lspsaga hover_doc ]]
--vim.api.nvim_command('autocmd CursorHoldI * silent! lua require("lspsaga.signaturehelp").signature_help()')

set_keymap('n', '<leader>ca', ':Lspsaga code_action<CR>', { noremap = true, silent= true })
set_keymap('n', '<leader>sd', ':Lspsaga hover_doc<CR>', { noremap = true, silent= true })
set_keymap("n", "gx", "<cmd>Lspsaga code_action<cr>", {silent = true, noremap = true})
set_keymap("n", "gh", "<cmd>Lspsaga lsp_finder<CR>", { silent = true })

-- Completion:
local luasnip = require('luasnip')
local lspkind = require('lspkind')

lspkind.init({ mode = true, preset = 'default' })

vim.opt.completeopt = "menuone,noselect"
local cmp = require('cmp')
cmp.setup {
  window = {
    completion = {
      winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,Search:None",
      col_offset = -3,
      side_padding = 0,
    },
  },
  view = {
    entries = {name = 'custom', selection_order = 'near_cursor' }
  },
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = {
    ['<C-p>']     = cmp.mapping.select_prev_item(),
    ['<C-n>']     = cmp.mapping.select_next_item(),
    ['<Up>']      = cmp.mapping.select_prev_item(),
    ['<Down>']    = cmp.mapping.select_next_item(),
    ['<C-d>']     = cmp.mapping.scroll_docs(-4),
    ['<C-f>']     = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>']     = cmp.mapping.close(),
    ['<Tab>']      = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end,
    ['<S-Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end,
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = "copilot" },
    { name = "nvim_lua" },
--    { name = "buffer" },
    { name = "path" },
  },
  formatting = {
    fields = { "kind", "abbr", "menu" },
    format = function(entry, vim_item)
      local kind = require("lspkind").cmp_format({ mode = "symbol_text", maxwidth = 50 })(entry, vim_item)
      local strings = vim.split(kind.kind, "%s", { trimempty = true })
      kind.kind = " " .. strings[1] .. " "
      kind.menu = "    (" .. strings[2] .. ")"

      return kind
    end,
  },
}

local dap, dapui = require("dap"), require("dapui")

dap.adapters.coreclr = {
  type = 'executable',
  command = '/path/to/dotnet/netcoredbg/netcoredbg',
  args = {'--interpreter=vscode'}
}

dap.configurations.cs = {
  {
    type = "coreclr",
    name = "launch - netcoredbg",
    request = "launch",
    program = function()
        return vim.fn.input('Path to dll', vim.fn.getcwd() .. '/bin/Debug/', 'file')
    end,
  },
}

dap.listeners.after.event_initialized["dapui_config"] = function()
  dapui.open()
end
dap.listeners.before.event_terminated["dapui_config"] = function()
  dapui.close()
end
dap.listeners.before.event_exited["dapui_config"] = function()
  dapui.close()
end

require("nvim-dap-virtual-text").setup()

require("neotest").setup({})

local comment = require('Comment')
comment.setup()

local commentft = require('Comment.ft')
commentft.set('yaml', '# %s')
         .set('javascript', {'// %s', '/* %s */'})
         .set('conf', '# %s')
         .set('fsharp', {'// %s', '(* %s *)'})
         .set('csharp', {'// %s', '/* %s */'})

require("pretty-fold").setup()
require("fold-preview").setup()
require("neogen").setup()
require("devcontainer").setup{}

require('scrlbkun').setup()

require('neoscroll').setup()

local wk = require("which-key")
wk.setup({})
wk.register({
    g = {
        name = "+Git",
        h = {
            name = "+Github",
            c = {
                name = "+Commits",
                c = { "<cmd>GHCloseCommit<cr>", "Close" },
                e = { "<cmd>GHExpandCommit<cr>", "Expand" },
                o = { "<cmd>GHOpenToCommit<cr>", "Open To" },
                p = { "<cmd>GHPopOutCommit<cr>", "Pop Out" },
                z = { "<cmd>GHCollapseCommit<cr>", "Collapse" },
            },
            i = {
                name = "+Issues",
                p = { "<cmd>GHPreviewIssue<cr>", "Preview" },
            },
            l = {
                name = "+Litee",
                t = { "<cmd>LTPanel<cr>", "Toggle Panel" },
            },
            r = {
                name = "+Review",
                b = { "<cmd>GHStartReview<cr>", "Begin" },
                c = { "<cmd>GHCloseReview<cr>", "Close" },
                d = { "<cmd>GHDeleteReview<cr>", "Delete" },
                e = { "<cmd>GHExpandReview<cr>", "Expand" },
                s = { "<cmd>GHSubmitReview<cr>", "Submit" },
                z = { "<cmd>GHCollapseReview<cr>", "Collapse" },
            },
            p = {
                name = "+Pull Request",
                c = { "<cmd>GHClosePR<cr>", "Close" },
                d = { "<cmd>GHPRDetails<cr>", "Details" },
                e = { "<cmd>GHExpandPR<cr>", "Expand" },
                o = { "<cmd>GHOpenPR<cr>", "Open" },
                p = { "<cmd>GHPopOutPR<cr>", "PopOut" },
                r = { "<cmd>GHRefreshPR<cr>", "Refresh" },
                t = { "<cmd>GHOpenToPR<cr>", "Open To" },
                z = { "<cmd>GHCollapsePR<cr>", "Collapse" },
            },
            t = {
                name = "+Threads",
                c = { "<cmd>GHCreateThread<cr>", "Create" },
                n = { "<cmd>GHNextThread<cr>", "Next" },
                t = { "<cmd>GHToggleThread<cr>", "Toggle" },
            },
        },
    },
}, { prefix = "<leader>" })
