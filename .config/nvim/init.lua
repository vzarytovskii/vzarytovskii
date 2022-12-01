
local ok, impatient = pcall(require, 'impatient')
if ok then
  impatient.enable_profile()
end

vim.wo.signcolumn = "yes"
vim.wo.number = true

local fn = vim.fn
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

local ok, packer = pcall(require, "packer")
local packer_bootstrap = false

if not ok then
  if fn.input("Packer seems to be missing. Download? (y for yes): ") ~= "y" then
    return
  end

  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

  print "...Cloning packer..."
  fn.delete(install_path, "rf")

  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    packer_bootstrap = true
  end

  vim.cmd [[packadd packer.nvim]]

  ok, packer = pcall(require, "packer")

  if ok then
    print "...Packer loaded successfully."
  else
    error("...Couldn't load packer !\nPacker path: " .. install_path .. "\n" .. packer)
  end
end

packer.startup({function(use)
  -- General
  use 'wbthomason/packer.nvim'
  use 'lewis6991/impatient.nvim'

  -- UI, theme & related:
  use 'olimorris/onedarkpro.nvim'

  use { 'nvim-telescope/telescope.nvim', requires = 'nvim-lua/plenary.nvim' }
  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build', requires = { 'nvim-telescope/telescope.nvim' } }

  -- Language specifics, including LSP, DAP, CMP, treesitter etc.
  use {
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',
      'WhoIsSethDaniel/mason-tool-installer.nvim',
      'neovim/nvim-lspconfig',
      'mfussenegger/nvim-dap',
      'jayp0521/mason-nvim-dap.nvim',
      'hrsh7th/nvim-cmp',
      'hrsh7th/cmp-nvim-lsp',
      'saadparwaiz1/cmp_luasnip',
      'lvimuser/lsp-inlayhints.nvim'
  }
  use { 'L3MON4D3/LuaSnip', requires = "rafamadriz/friendly-snippets" }
  use { 'jose-elias-alvarez/null-ls.nvim', requires = "nvim-lua/plenary.nvim" }
  use { 'SmiteshP/nvim-navic', requires = 'neovim/nvim-lspconfig' }
  use 'j-hui/fidget.nvim'
  use {
    'nvim-treesitter/nvim-treesitter',
    requires = {
      'nvim-treesitter/playground'
    },
    config = 'vim.cmd [[TSUpdate]]' 
  }
  use 'RRethy/vim-illuminate'

  use 'simrat39/rust-tools.nvim'
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
  return
end

local packer_group = vim.api.nvim_create_augroup('Packer', { clear = true })
vim.api.nvim_create_autocmd('BufWritePost', {
  command = 'source <afile> | PackerCompile',
  group = packer_group,
  pattern = vim.fn.expand '$MYVIMRC',
})

local function set_keymap(...) vim.api.nvim_set_keymap(...) end

function dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. dump(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end

local function get_tooling(t)
  local language_servers = {}
  local language_tools = {}
  local language_debuggers = {}
  for language, val in pairs(t) do
    if type(val.servers) == "table" then
      for server, v in pairs(val.servers) do
        language_servers[server] = v
      end
    end
    if type(val.tools) == "table" then
      for tool, v in pairs(val.tools) do
        language_tools[tool] = v
      end
    end
    if type(val.debuggers) == "table" then
      for k, v in pairs(val.debuggers) do
        language_debuggers[k] = v
      end
    end
  end
  return { tools = language_tools, servers = language_servers, debuggers = language_debuggers }
end

local onedarkpro = require("onedarkpro")
onedarkpro.setup({
  theme = "onedark_vivid",
  dark_theme = "onedark_vivid",
  light_theme = "onelight_vivid",
  caching = false
})
vim.cmd[[colorscheme onedarkpro]]

local nvim_runtime_path = vim.split(package.path, ';')
table.insert(nvim_runtime_path, 'lua/?.lua')
table.insert(nvim_runtime_path, 'lua/?/init.lua')

local languages = {
  others = {
    tools = { },
    servers = {
      diagnosticls = {},
    }
  },
  csharp = {
    tools = {},
    debuggers = { coreclr = {} },
    servers = {
      omnisharp = { use_mono = false }
    }
  },
  fsharp = {
    tools = { fantomas = {} },
    debuggers = { coreclr = {} },
    servers = {
      fsautocomplete = {
        cmd = { "fsautocomplete" },
        filetypes = { "fsharp" },
        init_options = {
          AutomaticWorkspaceInit = true
        }
      }
    }
  },
  lua = {
    servers = {
      sumneko_lua = {
        settings = {
          Lua = {
            runtime = {
              version = 'LuaJIT',
              path = nvim_runtime_path,
            },
            diagnostics = {
              globals = { 'vim' },
            },
            workspace = {
              checkThirdParty = false,
              library = vim.api.nvim_get_runtime_file('', true)
              },
            telemetry = { enable = false, },
          }
        }
      }
    }
  },
  rust = {
    debuggers = { cppdbg = {}, lldb = {}},
    servers = {
      rust_analyzer = {
        on_attach = function(client, bufnr)
          require("rust-tools").setup {
            tools = {
              inlay_hints = {
                auto = false,
              },
            },
          }
        end
      }
    }
  },
  yaml = {
    tools = { yamllint = {}, yamlfmt = {} },
    servers = {
      yamlls = {}
    }
  }
}

local function common_capabilities() 
  local status_ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")

  if status_ok then
    return cmp_nvim_lsp.default_capabilities()
  end

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
  return capabilities
end

local function configure_handlers(telescope_builtin)
  local config = {
    signs = {
      active = true,
      values = {
        { name = "DiagnosticSignError", text = "" },
        { name = "DiagnosticSignWarn", text = "" },
        { name = "DiagnosticSignHint", text = "" },
        { name = "DiagnosticSignInfo", text = "" },
      },
    },
    virtual_text = true,
    update_in_insert = false,
    underline = true,
    severity_sort = true,
    float = {
      focusable = false,
      style = "minimal",
      border = "rounded",
      source = "always",
      header = "",
      prefix = "",
      format = function(d)
        local code = d.code or (d.user_data and d.user_data.lsp.code)
        if code then
          return string.format("%s [%s]", d.message, code):gsub("1. ", "")
        end
        return d.message
      end,
    },
  },
  vim.diagnostic.config(config)
  vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, config.float)
  vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, config.float)
  --vim.lsp.handlers['textDocument/declaration'] = vim.lsp.with(telescope.builtins)
  vim.lsp.handlers['textDocument/definition'] = telescope_builtin.lsp_definitions
  --vim.lsp.handlers['textDocument/implementation'] = location_handler('LSP Implementations', opts.location),
  --vim.lsp.handlers['textDocument/typeDefinition'] = location_handler('LSP Type Definitions', opts.location),
  vim.lsp.handlers['textDocument/references'] = telescope_builtin.lsp_references
  --vim.lsp.handlers['textDocument/documentSymbol'] = symbol_handler('LSP Document Symbols', opts.symbol),
  --vim.lsp.handlers['workspace/symbol'] = symbol_handler('LSP Workspace Symbols', opts.symbol),
  --vim.lsp.handlers['callHierarchy/incomingCalls'] = call_hierarchy_handler('LSP Incoming Calls', 'from', opts.call_hierarchy),
  --vim.lsp.handlers['callHierarchy/outgoingCalls'] = call_hierarchy_handler('LSP Outgoing Calls', 'to', opts.call_hierarchy),
  --vim.lsp.handlers['textDocument/codeAction'] = code_action_handler('LSP Code Actions', opts.code_action),
end

local tooling = get_tooling(languages)

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
      "--trim"
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
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",
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

local mason = require('mason')
local mason_lspconfig = require('mason-lspconfig')
local nvim_lsp = require('lspconfig')
local mason_tool_installer = require('mason-tool-installer')
local mason_nvim_dap = require('mason-nvim-dap')
local null_ls = require('null-ls')
local navic = require('nvim-navic')
local nvim_cmp = require('cmp')
local luasnip = require('luasnip')
local treesitter = require('nvim-treesitter.configs')
local illuminate = require('illuminate')
local inlay_hints = require("lsp-inlayhints")

local capabilities = common_capabilities()

vim.o.winbar = "%{%v:lua.require'nvim-navic'.get_location()%}"

local fidget = require('fidget')

fidget.setup({
  text = {
    spinner = "dots"
  },
  align = {
    bottom = false,
    right = true,
  },
  fmt = {
    stack_upwards = false,
  },
  window = {
    relative = "editor"
  }
})

local luasnip = require('luasnip')
vim.opt.completeopt = {'menu', 'menuone', 'noselect'}
nvim_cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = {
    ['<CR>']      = nvim_cmp.mapping.confirm({select = false}),
    ['<C-p>']     = nvim_cmp.mapping.select_prev_item(),
    ['<C-n>']     = nvim_cmp.mapping.select_next_item(),
    ['<Up>']      = nvim_cmp.mapping.select_prev_item(),
    ['<Down>']    = nvim_cmp.mapping.select_next_item(),
    ['<C-d>']     = nvim_cmp.mapping.scroll_docs(-4),
    ['<C-f>']     = nvim_cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = nvim_cmp.mapping.complete(),
    ['<C-e>']     = nvim_cmp.mapping.close(),
    ['<Tab>']     = function(fallback)
      if nvim_cmp.visible() then
        nvim_cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end,
    ['<S-Tab>'] = function(fallback)
      if nvim_cmp.visible() then
        nvim_cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end,
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip' }
  }
}

mason.setup({
    PATH = "prepend",
    max_concurrent_installers = 4,
    providers = {
      "mason.providers.registry-api",
      "mason.providers.client"
  },
})

mason_lspconfig.setup({
  ensure_installed = vim.tbl_keys(tooling.servers),
  automatic_installation = true
})

mason_tool_installer.setup {
  ensure_installed = vim.tbl_keys(tooling.tools),
  auto_update = true,
  run_on_start = true
}

mason_nvim_dap.setup({
    ensure_installed = vim.tbl_keys(tooling.debuggers),
    automatic_installation = true,
    automatic_setup = true
})

null_ls.setup({
    sources = {
        null_ls.builtins.code_actions.gitsigns,
        null_ls.builtins.code_actions.refactoring,
        null_ls.builtins.completion.luasnip,
    },
})

local lsp_formatting = function(bufnr)
    vim.lsp.buf.format({
        filter = function(client)
            return client.name == "null-ls"
        end,
        bufnr = bufnr,
    })
end

local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

inlay_hints.setup()

local on_attach = function(client, bufnr)
    if client.server_capabilities.documentSymbolProvider then
        navic.attach(client, bufnr)
    end

    inlay_hints.on_attach(client, bufnr)

    if client.supports_method("textDocument/formatting") then
        vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
        vim.api.nvim_create_autocmd("BufWritePre", {
            group = augroup,
            buffer = bufnr,
            callback = function()
                lsp_formatting(bufnr)
            end,
        })
    end

    client.server_capabilities.documentFormattingProvider = false
    client.server_capabilities.documentRangeFormattingProvider = false
end

mason_lspconfig.setup_handlers {
    function (server_name)
        local server_opts = tooling.servers[server_name] or {}
        local server_on_attach = server_opts[on_attach] or function(_, _) end

        local on_attach_fn = function(client, bufnr)
          on_attach(client, bufnr)
          server_on_attach(client, bufnr)
        end

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
}

configure_handlers(telescope_builtin)

treesitter.setup {
  -- TODO: Add this to overall langauges config, per language.
  ensure_installed = { "lua", "rust", "c_sharp", "yaml" },
  sync_install = false,
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

require('illuminate').configure({
  providers = {
        'treesitter',
        'lsp',
        'regex',
    },
  delay = 50
})

