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

  -- Language specifics, including LSP, DAP, CMP, etc.
  use {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      'WhoIsSethDaniel/mason-tool-installer.nvim',
      "neovim/nvim-lspconfig",
      "mfussenegger/nvim-dap",
      "jayp0521/mason-nvim-dap.nvim",
  }
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

local nvim_runtime_path = vim.split(package.path, ';')
table.insert(nvim_runtime_path, 'lua/?.lua')
table.insert(nvim_runtime_path, 'lua/?/init.lua')

local languages = {
  others = {
    tools = { prettier = {} },
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
            workspace = { library = vim.api.nvim_get_runtime_file('', true) },
            telemetry = { enable = false, },
          }
        }
      }
    }
  },
  rust = {
    debuggers = { cppdbg = {}, lldb = {}},
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

local function configure_languages(languages)
  local tooling = get_tooling(languages)

  local mason = require('mason')
  local mason_lspconfig = require('mason-lspconfig')
  local nvim_lsp = require('lspconfig')
  local mason_tool_installer = require('mason-tool-installer')
  local mason_nvim_dap = require("mason-nvim-dap")

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

end

configure_languages(languages)
