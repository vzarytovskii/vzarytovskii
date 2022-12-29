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

---@diagnostic disable-next-line: redefined-local
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
  use 'nvim-lua/plenary.nvim'

  -- UI, theme & related:
  use 'olimorris/onedarkpro.nvim'
  use 'kyazdani42/nvim-web-devicons'

  use { 'nvim-telescope/telescope.nvim', requires = 'nvim-lua/plenary.nvim' }
  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build', requires = { 'nvim-telescope/telescope.nvim' } }
  use {'nvim-telescope/telescope-ui-select.nvim', requires = { 'nvim-telescope/telescope.nvim' } }
  use {'debugloop/telescope-undo.nvim', requires = { 'nvim-telescope/telescope.nvim' } }

  -- Language specifics, including LSP, DAP, CMP, treesitter etc.
  use {
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',
      'WhoIsSethDaniel/mason-tool-installer.nvim',
      'neovim/nvim-lspconfig'
  }

  use {
      'mfussenegger/nvim-dap',
      'jayp0521/mason-nvim-dap.nvim',
      'rcarriga/nvim-dap-ui'
  }

  use {
      'hrsh7th/nvim-cmp',
      'hrsh7th/cmp-nvim-lsp',
      'saadparwaiz1/cmp_luasnip',
  }

  use { "onsails/lspkind.nvim" }
  use 'https://git.sr.ht/~whynothugo/lsp_lines.nvim'
  use 'lvimuser/lsp-inlayhints.nvim'

  use { 'L3MON4D3/LuaSnip', requires = "rafamadriz/friendly-snippets" }
  use { 'jose-elias-alvarez/null-ls.nvim', requires = "nvim-lua/plenary.nvim" }
  use { "utilyre/barbecue.nvim",
        branch = "dev",
        requires = {
        "neovim/nvim-lspconfig",
        "smiteshp/nvim-navic",
        "kyazdani42/nvim-web-devicons",
      }
  }
  use 'j-hui/fidget.nvim'
  use {
    'nvim-treesitter/nvim-treesitter',
    requires = {
      'nvim-treesitter/playground'
    },
    config = 'vim.cmd [[TSUpdate]]'
  }
  use 'RRethy/vim-illuminate'
  use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } }

  use { 'antoinemadec/FixCursorHold.nvim' }

  use { 'stevearc/aerial.nvim' }
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

local function dump(o)
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
  local language_debuggers_adapters = {}
  local language_debuggers_settings = {}
  for _, val in pairs(t) do
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
    if type(val["debuggers"]) == "table" then
      if type(val["debuggers"]["adapters"]) == "table" then
        for k, v in pairs(val.debuggers.adapters) do
          language_debuggers_adapters[k] = v
        end
      end
      if type(val["debuggers"]["settings"]) == "function" then
        table.insert(language_debuggers_settings, val.debuggers.settings)
      end
    end
  end
  return { tools = language_tools, servers = language_servers, debuggers_adapters = language_debuggers_adapters, debuggers_settings = language_debuggers_settings }
end

local function set_common_settings()
  vim.g.mapleader = " "
  vim.g.netrw_browse_split = 0
  vim.g.netrw_banner = 0
  vim.g.netrw_winsize = 25
  vim.opt.guicursor = ""

  vim.opt.nu = true
  vim.opt.relativenumber = false

  vim.opt.tabstop = 4
  vim.opt.softtabstop = 4
  vim.opt.shiftwidth = 2
  vim.opt.expandtab = true

  vim.opt.smartindent = true

  vim.opt.wrap = false

  vim.opt.swapfile = false
  vim.opt.backup = false
  vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
  vim.opt.undofile = true

  vim.opt.hlsearch = false
  vim.opt.incsearch = true

  vim.opt.termguicolors = true

  vim.opt.scrolloff = 8
  vim.opt.signcolumn = "yes"
  vim.opt.isfname:append("@-@")

  vim.opt.updatetime = 50

  vim.opt.colorcolumn = "80"

  vim.api.nvim_command('autocmd BufNewFile,BufRead *.fs,*.fsx,*.fsi,*.fsl,*.fsy set filetype=fsharp')
end

set_common_settings()

local onedarkpro = require("onedarkpro")
onedarkpro.setup({
  caching = false
})
vim.cmd[[colorscheme onelight]]

local nvim_runtime_path = vim.split(package.path, ';')
table.insert(nvim_runtime_path, 'lua/?.lua')
table.insert(nvim_runtime_path, 'lua/?/init.lua')

local languages = {
  others = {
    tools = { },
    servers = {}
  },
  csharp = {
    tools = {},
    debuggers = {
      adapters = {
        coreclr = function (dap)
          dap.adapters.coreclr = {
            type = 'executable',
            command = 'netcoredbg',
            args = {'--interpreter=vscode'}
          }
        end,
      },
      settings = function (dap)
        dap.configurations.cs = {
          {
            type = "coreclr",
            name = "launch - netcoredbg",
            request = "launch",
            program = function()
                return vim.fn.input('Path to dll', vim.fn.getcwd(), 'file')
            end,
          },
        }
      end,
    },
    servers = {
      omnisharp = { use_mono = false }
    }
  },
  fsharp = {
    tools = { fantomas = {} },
    debuggers = {
      adapters = {
        coreclr = function (dap)
          dap.adapters.coreclr = {
            type = 'executable',
            command = 'netcoredbg',
            args = {'--interpreter=vscode'}
          }
        end,
      },
      settings = function (dap)
        dap.configurations.fs = {
          {
            type = "coreclr",
            name = "launch - netcoredbg",
            request = "launch",
            program = function()
                return vim.fn.input('Path to dll', vim.fn.getcwd(), 'file')
            end,
          },
        }
      end,
    },
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
    debuggers = {
      adapters = {
        codelldb = function (dap)
          dap.adapters.codelldb = {
            type = 'server',
            port = "${port}",
            executable = {
              command = 'codelldb',
              args = {"--port", "${port}"},
            }
          }
        end,
      },
      settings = function (dap)
        dap.configurations.rust = {
          {
            name = "Launch...",
            type = "codelldb",
            request = "launch",
            program = function()
              return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
            end,
            cwd = '${workspaceFolder}',
            stopOnEntry = false,
          },
        }
      end,
    },
    servers = {
      rust_analyzer = {}
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

  if status_ok then
    return cmp_nvim_lsp.default_capabilities(capabilities)
  end

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
    virtual_lines = true,
    update_in_insert = false,
    underline = true,
    severity_sort = true,
    float = {
      focus = false,
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
  }

  vim.diagnostic.config(config)
  vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, config.float)
  vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, config.float)
  vim.lsp.handlers['textDocument/declaration'] = vim.lsp.with(vim.lsp.buf.declaration, config.float)
  vim.lsp.handlers['textDocument/definition'] = telescope_builtin.lsp_definitions
  vim.lsp.handlers['textDocument/documentSymbol'] = telescope_builtin.lsp_document_symbols
  vim.lsp.handlers['textDocument/references'] = telescope_builtin.lsp_references
  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
      virtual_text = config.virtual_text,
      update_in_insert = true,
      show_diagnostic_autocmds = { 'InsertLeave', 'TextChanged', "TextYankPost", "CursorHold" },
    }
  )
  --vim.lsp.handlers['textDocument/implementation'] = location_handler('LSP Implementations', opts.location),
  --vim.lsp.handlers['textDocument/typeDefinition'] = location_handler('LSP Type Definitions', opts.location),
  --vim.lsp.handlers['workspace/symbol'] = symbol_handler('LSP Workspace Symbols', opts.symbol),
  --vim.lsp.handlers['callHierarchy/incomingCalls'] = call_hierarchy_handler('LSP Incoming Calls', 'from', opts.call_hierarchy),
  --vim.lsp.handlers['callHierarchy/outgoingCalls'] = call_hierarchy_handler('LSP Outgoing Calls', 'to', opts.call_hierarchy),
  --vim.lsp.handlers['textDocument/codeAction'] = code_action_handler('LSP Code Actions', opts.code_action)
  vim.g.cursorhold_updatetime = 100
  vim.cmd [[autocmd CursorHold,CursorHoldI * :lua vim.diagnostic.open_float() ]]

end

local lsp_keybinds = function(bufnr)
  local opts = { noremap = false, silent = true }
  vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<C-h>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>ds", "<cmd>lua vim.lsp.buf.document_symbol()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "i", "<C-;>", "<cmd>lua vim.lsp.buf.document_symbol()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>ws", "<cmd>lua vim.lsp.buf.workspace_symbol()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gI", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gl", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gs", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "i", "<C-.>", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
end

local tooling = get_tooling(languages)

local telescope = require('telescope')
local telescope_actions = require('telescope.actions')
local telescope_builtin = require('telescope.builtin')

telescope.setup {
  defaults = {
    layout_strategy = 'flex',
    layout_config = {
      width = 0.75,
      height = 0.95
    },
    file_sorter = require("telescope.sorters").get_fuzzy_file,
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
    find_command = { "fdfind", "--type", "f", "--strip-cwd-prefix" },
    file_ignore_patterns = {
        ".git/",
        ".vscode/",
        "node_modules",
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
    buffers = {
      preview = true,
      only_cwd = false,
      show_all_buffers = false,
      ignore_current_buffer = true,
      sort_lastused = true,
      theme = "dropdown",
      sorter = require("telescope.sorters").get_substr_matcher(),
      selection_strategy = "closest",
      path_display = {"smart"},
      layout_strategy = "center",
      winblend = 0,
      layout_config = {width = 70},
    }
  },
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",
    },
    ["ui-select"] = {
      require("telescope.themes").get_dropdown {
        layout_strategy = "cursor",
      }
    },
    aerial = {
      show_nesting = {
        ['_'] = false, -- This key will be the default
        json = true,
        yaml = true,
      }
    }
  }
}

telescope.load_extension("fzf")
telescope.load_extension("ui-select")
telescope.load_extension("undo")
telescope.load_extension('aerial')

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
set_keymap('n', '<C-u>', ':Telescope undo<CR>', { noremap = true, silent= true })

local mason = require('mason')
local mason_lspconfig = require('mason-lspconfig')
local nvim_lsp = require('lspconfig')
local mason_tool_installer = require('mason-tool-installer')
local mason_nvim_dap = require('mason-nvim-dap')
local null_ls = require('null-ls')
local barbecue = require('barbecue')
local barbecue_ui = require("barbecue.ui")
local navic = require('nvim-navic')
local nvim_cmp = require('cmp')
local luasnip = require('luasnip')
local treesitter = require('nvim-treesitter.configs')
local illuminate = require('illuminate')
local inlay_hints = require("lsp-inlayhints")
local dap = require('dap')
local dapui = require('dapui')

dapui.setup()

barbecue.setup({
  create_autocmd = false,
  show_modified = true,
  attach_navic = false
})
vim.api.nvim_create_autocmd({
  "WinScrolled",
  "BufWinEnter",
  "CursorHold",
  "InsertLeave",
  "BufWritePost",
  "TextChanged",
  "TextChangedI",
}, {
  group = vim.api.nvim_create_augroup("barbecue", {}),
  callback = function()
    barbecue_ui.update()
  end,
})

inlay_hints.setup()

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
  highlight = {
    enable = true,
    disable = { "markdown" },
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

local capabilities = common_capabilities()

local fidget = require('fidget')

fidget.setup({
  text = {
    spinner = "dots"
  },
  align = {
    bottom = true,
    right = true,
  },
  fmt = {
    stack_upwards = false,
  },
  window = {
    relative = "win" -- win or editor
  }
})

vim.opt.completeopt = {'menu', 'menuone', 'noselect'}
nvim_cmp.setup {
  enabled = function()
    -- disable completion in comments
    local context = require 'cmp.config.context'
    -- keep command mode completion enabled when cursor is in a comment
    if vim.api.nvim_get_mode().mode == 'c' then
      return true
    else
      return not context.in_treesitter_capture("comment")
        and not context.in_syntax_group("Comment")
    end
  end,
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
  },
  window = {
    completion = {
      winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,Search:None",
      col_offset = -3,
      side_padding = 0,
    },
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
    ensure_installed = vim.tbl_keys(tooling.debuggers_adapters),
    automatic_installation = true,
    automatic_setup = true
})

local setup_dap = function(dap)
  for _, cb in pairs(tooling.debuggers_adapters) do
    if type(cb) == "function" then
      cb(dap)
    end
  end
  for _, cb in pairs(tooling.debuggers_settings) do
    if type(cb) == "function" then
      cb(dap)
    end
  end
end

setup_dap(dap)

null_ls.setup({
    sources = {
        null_ls.builtins.code_actions.gitsigns,
        null_ls.builtins.completion.luasnip,
        null_ls.builtins.completion.tags,
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

configure_handlers(telescope_builtin)

local global_on_attach = function(client, bufnr)
    if client.server_capabilities.documentSymbolProvider then
        navic.attach(client, bufnr)
    end

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

    if client.server_capabilities.code_lens then
      local codelens = vim.api.nvim_create_augroup(
        'LSPCodeLens',
        { clear = true }
      )
      vim.api.nvim_create_autocmd({ 'BufEnter' }, {
        group = codelens,
        callback = function()
          vim.lsp.codelens.refresh()
        end,
        buffer = bufnr,
        once = true,
      })
      vim.api.nvim_create_autocmd({ 'BufWritePost', 'CursorHold' }, {
        group = codelens,
        callback = function()
          vim.lsp.codelens.refresh()
        end,
        buffer = bufnr,
      })
    end

    client.server_capabilities.documentFormattingProvider = false
    client.server_capabilities.documentRangeFormattingProvider = false

    inlay_hints.on_attach(client, bufnr)

    lsp_keybinds(bufnr)
end

illuminate.configure({
  providers = {
        'lsp',
        'treesitter',
        'regex',
    },
  delay = 50
})

mason_lspconfig.setup_handlers {
    function (server_name)
        local server_opts = tooling.servers[server_name] or {}
        local server_on_attach = server_opts["on_attach"] or function(_, _) end
        local server_settings = server_opts["settings"] or {}
        local server_capabilities = server_opts["capabilities"] or {}

        local composed_on_attach_fn = function(client, bufnr)
          server_on_attach(client, bufnr)
          global_on_attach(client, bufnr)
        end

        local opts = {
          on_attach = composed_on_attach_fn,
          capabilities = merge(capabilities, server_capabilities),
	        settings = server_settings,
          flags = {
            debounce_text_changes = 150,
          }
        }

        nvim_lsp[server_name].setup(opts)
    end,
}

local gitsigns = require('gitsigns')
gitsigns.setup {
  signcolumn = true,
  numhl      = true,
  linehl     = false,
  word_diff  = true,
  current_line_blame = false,
  current_line_blame_opts = {
    virt_text = true,
    virt_text_pos = 'right_align', -- 'eol' | 'overlay' | 'right_align'
    delay = 1000,
    ignore_whitespace = false,
  },
}

local aerial = require('aerial')
aerial.setup({
  on_attach = function(bufnr)
    aerial.open_all()
  end,
  backends = { "treesitter", "lsp", "markdown", "man" },
  -- close_automatic_events = { "unfocus", "switch_buffer", "unsupported" },
  open_automatic = true,
  show_guides = true,
  attach_mode = "window",
  layout = {
    max_width = { 50, 0.5 },
    width = nil,
    min_width = 10,
    win_opts = {},
    default_direction = "float",
    placement = "window",

    preserve_equality = false,
  },
  guides = {
    mid_item = "├─",
    last_item = "└─",
    nested_top = "│",
    whitespace = "  ",
  },
  float = {
    max_height = 0.9,
    relative = "editor",
    override = function(conf, source_winid)
      local padding = 1
      conf.anchor = 'NE'
      conf.row = padding
      conf.col = vim.o.columns - padding
      return conf
    end,
  }
})
