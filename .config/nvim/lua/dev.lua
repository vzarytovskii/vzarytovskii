vim.api.nvim_command('autocmd BufNewFile,BufRead *.fs,*.fsx,*.fsi,*.fsl,*.fsy set filetype=fsharp')

vim.opt.tabstop=8
vim.opt.shiftwidth=2
vim.opt.expandtab=true
vim.opt.smartindent=true

local ok, treesitter = pcall(require, "nvim-treesitter.configs")

if not ok then
  return
end

require('gitsigns').setup()
require('octo').setup()
require('neogit').setup()

treesitter.setup {
  ensure_installed = "all",
  sync_install = false,
  highlight = {
    enable = true,
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

local config = {
    virtual_text = true,
    signs = {
      active = signs,
    },
    update_in_insert = true,
    underline = true,
    severity_sort = true,
    float = {
      focusable = false,
      style = "minimal",
      border = "rounded",
      source = "always",
      header = "",
      prefix = "",
    },
}

-- vim.diagnostic.config(config)

local lsp_signature = require 'lsp_signature'

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.preselectSupport = true
capabilities.textDocument.completion.completionItem.insertReplaceSupport = true
capabilities.textDocument.completion.completionItem.labelDetailsSupport = true
capabilities.textDocument.completion.completionItem.deprecatedSupport = true
capabilities.textDocument.completion.completionItem.commitCharactersSupport = true
capabilities.textDocument.completion.completionItem.tagSupport = { valueSet = { 1 } }
capabilities.textDocument.completion.completionItem.resolveSupport = {
   properties = {
      "documentation",
      "detail",
      "additionalTextEdits",
   },
}
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

local function lsp_highlight_document(client)
  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec(
      [[
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]],
      false
    )
  end
end

local virtualtypes = require 'virtualtypes'

local on_attach = function(client, bufnr)
  lsp_signature.on_attach({
        bind = true,
        floating_window = true,
  })

  virtualtypes.on_attach(client, bufnr)

  -- local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
  lsp_highlight_document(client)

  -- local opts = { noremap=true, silent=true }
--[[
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
]]--
end


local nvim_lsp = require('lspconfig')
local lsp_installer_servers = require('nvim-lsp-installer.servers')
local servers = {
        hls = {},
        grammarly = {},
        sumneko_lua = {},
        fsautocomplete = {
          cmd = { "fsautocomplete" },
          filetypes = { "fsharp" },
          init_options = {
             AutomaticWorkspaceInit = true
          },
        },
        csharp_ls = {}
}

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

for server_name, server_opts in pairs(servers) do
  local server_available, server = lsp_installer_servers.get_server(server_name)
  if server_available then
        if not server:is_installed() then
            print("Server ", server_name, " is not installed. Installing...")
            server:install()
        end
        server:on_ready(function ()
            local opts = {
                on_attach = on_attach,
                capabilities = capabilities,
                flags = {
                        debounce_text_changes = 150,
                }
            }

            merge(opts, server_opts)
            nvim_lsp[server_name].setup(opts)
            server:setup(opts)
        end)
    else
        error("No server available for: " .. server_name .. "\n")
    end
end

require('lspsaga').init_lsp_saga()
local function set_keymap(...) vim.api.nvim_set_keymap(...) end
set_keymap('n', 'K', ':Lspsaga hover_doc<CR>', { noremap = true, silent = true })
set_keymap('n', 'gh', ':Lspsaga lsp_finder<CR>', { noremap = true, silent = true })
set_keymap('n', 'gd', ':Lspsaga preview_definition<CR>', { noremap = true, silent = true })
set_keymap('n', '<leader>ca', ':Lspsaga code_action<CR>', { noremap = true, silent= true })
set_keymap('v', '<leader>ca', ':<C-U>Lspsaga range_code_action<CR>', { noremap = true, silent= true })
set_keymap("n", "gx", "<cmd>Lspsaga code_action<cr>", {silent = true, noremap = true})
set_keymap("x", "gx", ":<c-u>Lspsaga range_code_action<cr>", {silent = true, noremap = true})
set_keymap("n", "K",  "<cmd>Lspsaga hover_doc<cr>", {silent = true, noremap = true})
set_keymap("n", "go", "<cmd>Lspsaga show_line_diagnostics<cr>", {silent = true, noremap = true})
set_keymap("n", "gj", "<cmd>Lspsaga diagnostic_jump_next<cr>", {silent = true, noremap = true})
set_keymap("n", "gk", "<cmd>Lspsaga diagnostic_jump_prev<cr>", {silent = true, noremap = true})
--buf_set_keymap("n", "<C-u>", "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<cr>", nil)
--buf_set_keymap("n", "<C-d>", "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<cr>", nil)

local null_ls = require("null-ls")
null_ls.setup({
    sources = {
        -- null_ls.builtins.completion.spell,
        null_ls.builtins.code_actions.gitsigns,
    },
})

vim.o.completeopt = 'menuone,noselect'

local luasnip = require('luasnip')

local cmp = require('cmp')
cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = function(fallback)
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
    { name = "nvim_lua" },
    { name = "buffer" },
    { name = "path" },
  },
}

require "lsp_signature".setup()

-- require("trouble").setup()

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

local comment = require('Comment')
comment.setup()

local commentft = require('Comment.ft')
commentft.set('yaml', '# %s')
         .set('javascript', {'// %s', '/* %s */'})
         .set('conf', '# %s')
         .set('fsharp', {'// %s', '(* %s *)'})
         .set('csharp', {'// %s', '/* %s */'})

require("pretty-fold").setup()
require("pretty-fold.preview").setup()

require("neogen").setup()
