local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=main",
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)

vim.g.mapleader = " "
vim.wo.signcolumn = "yes"
vim.wo.number = true
vim.opt.completeopt = {'menu', 'menuone', 'noselect'}
vim.o.foldcolumn = '1'
vim.o.foldlevel = 99
vim.o.foldlevelstart = 99
vim.o.foldenable = true

vim.opt.guicursor = ""

vim.o.cursorline = true
vim.opt.number = true
vim.opt.relativenumber = true

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.exrc = true

vim.g.lsp_zero_extend_lspconfig = 0


vim.opt.smartindent = true

vim.opt.wrap = false

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.undofile = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.scrolloff = 8
vim.opt.signcolumn = "number"
vim.opt.isfname:append("@-@")

vim.opt.updatetime = 50

vim.opt.colorcolumn = "80"

vim.cmd [[ set clipboard+=unnamedplus ]]

vim.api.nvim_command('autocmd BufNewFile,BufRead *.fs,*.fsx,*.fsi,*.fsl,*.fsy set filetype=fsharp')
vim.api.nvim_command('autocmd BufNewFile,BufRead *.fsproj,*.csproj,*.vbproj,*.cproj,*.proj set filetype=xml')

local function has_words_before()
  if vim.api.nvim_buf_get_option(0, "buftype") == "prompt" then return false end
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_text(0, line-1, 0, line-1, col, {})[1]:match("^%s*$") == nil
end

local function get_diagnostic_start(diagnostic_entry)
  return diagnostic_entry['lnum'], diagnostic_entry['col']
end

local function get_diagnostic_end(diagnostic_entry)
  return diagnostic_entry['end_lnum'], diagnostic_entry['end_col']
end

local function in_range(cursor_line, cursor_char)
  return function(diagnostic)
    local start_line, start_char = get_diagnostic_start(diagnostic)
    local end_line, end_char = get_diagnostic_end(diagnostic)

    local one_line_diag = start_line == end_line

    if one_line_diag and start_line == cursor_line then
      if cursor_char >= start_char and cursor_char < end_char then
        return true
      end

    -- multi line diagnostic
    else
      if cursor_line == start_line and cursor_char >= start_char then
        return true
      elseif cursor_line == end_line and cursor_char < end_char then
        return true
      elseif cursor_line > start_line and cursor_line < end_line then
        return true
      end
    end

    return false
  end
end

local function open_diagnostics_if_no_float()
    for _, winid in pairs(vim.api.nvim_tabpage_list_wins(0)) do
      if vim.api.nvim_win_get_config(winid).zindex then
        return
      end
    end
    vim.diagnostic.open_float(0, {
      scope = "cursor",
      focusable = false,
      close_events = {
        "CursorMoved",
        "CursorMovedI",
        "BufHidden",
        "InsertCharPre",
        "WinLeave",
      },
    })
end

function hover_handler(client, bufnr)
  local opts = { focus=false, scope="cursor" }
  local winid = require('ufo').peekFoldedLinesUnderCursor()
  if not winid then
      local pos = vim.api.nvim_win_get_cursor(0)
      local line_nr = pos[1] - 1
      local column_nr = pos[2]
      local diagnostic_under_cursor =
        vim.tbl_filter(in_range(line_nr, column_nr), vim.diagnostic.get(bufnr, client))

      if rawequal(next(diagnostic_under_cursor), nil) then
        -- vim.lsp.buf.hover(nil, opts)
      else
        -- vim.diagnostic.open_float(nil, opts)
        -- TODO: Add showing diagnostics, and maybe some additional info based on buffers (like show commit info or something in neogit, or show titles of GH issues/PRs in comments/buffers)
        -- render_diagnostic_window(diagnostic_under_cursor[0] or diagnostic_under_cursor[1], opts)
        open_diagnostics_if_no_float()
      end
  end
end

vim.api.nvim_create_autocmd(
  'LspAttach',
  {
    group = vim.api.nvim_create_augroup('UserLspConfig', {}),
    callback = function(ev)

      local telescope_builtin = require('telescope.builtin')
      local glance = require('glance.lsp')

      local config = {
        signs = {
          active = true,
          values = {
            { name = "DiagnosticSignError", text = "[E]" },
            { name = "DiagnosticSignWarn", text = "[W]" },
            { name = "DiagnosticSignHint", text = "[H]" },
            { name = "DiagnosticSignInfo", text = "[I]" },
          },
        },
        virtual_text = false,
        virtual_lines = { only_current_line = true },
        update_in_insert = false,
        underline = true,
        severity_sort = true,
        float = {
          max_width = 120,
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

      vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

      vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, config.float)
      vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, config.float)
      vim.lsp.handlers['textDocument/declaration'] = vim.lsp.with(vim.lsp.buf.declaration, config.float)
      vim.lsp.handlers['textDocument/definition'] = telescope_builtin.lsp_definitions
      vim.lsp.handlers['textDocument/references'] = telescope_builtin.lsp_references
      vim.lsp.handlers['textDocument/documentSymbol'] = telescope_builtin.lsp_document_symbols
      vim.lsp.handlers['workspace/symbol'] = telescope_builtin.lsp_workspace_symbols
      vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
        vim.lsp.diagnostic.on_publish_diagnostics, {
          virtual_text = config.virtual_text,
          update_in_insert = true,
          show_diagnostic_autocmds = { 'InsertLeave', 'TextChanged', "TextYankPost", "CursorHold" },
        }
      )

      local opts = { buffer = ev.buf }
      vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
      vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
      vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
      vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
      vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
      vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
      vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
      vim.keymap.set('n', '<space>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
      end, opts)
      vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
      vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
      vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
      vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
      vim.keymap.set('n', '<space>f', function() vim.lsp.buf.format { async = true } end, opts)

      -- Explicity set for Glance, since it doesn't seem to have an API to set the handler
      local options = { noremap = false, silent = true }
      vim.api.nvim_buf_set_keymap(ev.buf, 'n', 'gd', '<CMD>Glance definitions<CR>', options)
      vim.api.nvim_buf_set_keymap(ev.buf, 'n', 'gr', '<CMD>Glance references<CR>', options)
      vim.api.nvim_buf_set_keymap(ev.buf, 'n', 'gy', '<CMD>Glance type_definitions<CR>', options)
      vim.api.nvim_buf_set_keymap(ev.buf, 'n', 'gm', '<CMD>Glance implementations<CR>', options)

      vim.api.nvim_buf_set_keymap(ev.buf, 'n', 'gpd', "<cmd>lua require('goto-preview').goto_preview_definition()<CR>", options)
      vim.api.nvim_buf_set_keymap(ev.buf, 'n', 'gpt', "<cmd>lua require('goto-preview').goto_preview_type_definition()<CR>", options)
      vim.api.nvim_buf_set_keymap(ev.buf, 'n', 'gpi', "<cmd>lua require('goto-preview').goto_preview_implementation()<CR>", options)
      vim.api.nvim_buf_set_keymap(ev.buf, 'n', 'gpD', "<cmd>lua require('goto-preview').goto_preview_declaration()<CR>", options)
      vim.api.nvim_buf_set_keymap(ev.buf, 'n', 'gP',  "<cmd>lua require('goto-preview').close_all_win()<CR>", options)
      vim.api.nvim_buf_set_keymap(ev.buf, 'n', 'gpr', "<cmd>lua require('goto-preview').goto_preview_references()<CR>", options)

    end,
})

require("lazy").setup({
  { 'antoinemadec/FixCursorHold.nvim' },
  {
    'projekt0n/github-nvim-theme',
    lazy = false,
    priority = 1000,
    config = function()
      vim.opt.termguicolors = true
      --vim.opt.background = "light"
      require('github-theme').setup({
        options = {
          compile_path = vim.fn.stdpath('cache') .. '/github-theme',
          compile_file_suffix = '_compiled', -- Compiled file suffix
          hide_end_of_buffer = true, -- Hide the '~' character at the end of the buffer for a cleaner look
          hide_nc_statusline = true, -- Override the underline style for non-active statuslines
          transparent = false,       -- Disable setting background
          terminal_colors = true,    -- Set terminal colors (vim.g.terminal_color_*) used in `:terminal`
          dim_inactive = false,      -- Non focused panes set to alternative background
          module_default = true,     -- Default enable value for modules
          styles = {                 -- Style to be applied to different syntax groups
            comments = 'NONE',       -- Value is any valid attr-list value `:help attr-list`
            functions = 'NONE',
            keywords = 'NONE',
            variables = 'NONE',
            conditionals = 'NONE',
            constants = 'NONE',
            numbers = 'NONE',
            operators = 'NONE',
            strings = 'NONE',
            types = 'NONE',
          },
          inverse = {                -- Inverse highlight for different types
            match_paren = false,
            visual = false,
            search = false,
          },
          darken = {                 -- Darken floating windows and sidebar-like windows
            floats = false,
            sidebars = {
              enable = true,
              list = {},             -- Apply dark background to specific windows
            },
          },
          modules = {                -- List of various plugins and additional options
            -- ...
          },
        },
        palettes = {},
        specs = {},
        groups = {},
      })
      -- vim.cmd([[colorscheme github_light_high_contrast]])
      require('auto-dark-mode').init()
    end,
    dependencies = { "f-person/auto-dark-mode.nvim" }
  },
  {
    "f-person/auto-dark-mode.nvim",
    config = {
      update_interval = 1000,
      set_dark_mode = function()
        vim.api.nvim_set_option("background", "dark")
        vim.cmd("colorscheme github_dark_high_contrast")
      end,
      set_light_mode = function()
        vim.api.nvim_set_option("background", "light")
        vim.cmd("colorscheme github_light_high_contrast")
      end,
    },
  },
  {
    'hoob3rt/lualine.nvim',
    lazy = false,
    config = function ()
      require('lualine').setup {
        options = {
          icons_enabled = false
        }
      }
    end
  },
  {
    'nvim-lua/plenary.nvim',
    lazy = true
  },
  {
    'nvim-telescope/telescope.nvim',
     dependencies = { 'nvim-lua/plenary.nvim' },
     config = function()
      local telescope = require('telescope')
      local telescope_actions = require('telescope.actions')
      local telescope_builtin = require('telescope.builtin')
      telescope.setup {
        defaults = {
          file_ignore_patterns = { "node_modules", ".git", ".vscode", ".artifacts" },
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
            find_command = { "fdfind", "--type", "f", "--strip-cwd-prefix" },
          }
        }
      }

      vim.keymap.set({'n', 'v', 'i'}, '<M-f>', telescope_builtin.find_files, {})
      vim.keymap.set({'n', 'v', 'i'}, '<M-s>', telescope_builtin.live_grep, {})
     end
  },
  {
    'gelguy/wilder.nvim',
    event = 'UiEnter',
    config = function ()
      local wilder = require('wilder')
      wilder.setup({
        modes = {':', '/', '?'},
        next_key = '<Tab>',
        previous_key = '<S-Tab>',
      })
    end
  },
  {
    'lewis6991/satellite.nvim',
    event = { 'BufEnter' },
    config = function ()
      require('satellite').setup {
        current_only = true,
        winblend = 50,
        zindex = 40,
        excluded_filetypes = {},
        width = 3,
        handlers = {
          cursor = {
            enable = true,
            symbols = { '⎺', '⎻', '⎼', '⎽' }
          },
          search = {
            enable = true,
          },
          diagnostic = {
            enable = true,
            signs = {'-', '=', '≡'},
            min_severity = vim.diagnostic.severity.HINT,
          },
          gitsigns = {
            enable = true,
            signs = {
              add = "+",
              change = "*",
              delete = "-",
            }
          },
          marks = {
            enable = true,
            show_builtins = false, -- shows the builtin marks like [ ] < >
            key = 'm'
          },
          quickfix = {
            signs = { '-', '=', '≡' },
          }
        },
      }
    end
  },
  {
    'stevearc/oil.nvim',
    lazy = false,
    config = function()
      require('oil').setup({
        columns = { 'size', 'permissions', 'mtime' },
        default_file_explorer = true,
        use_default_keymaps = true,
        view_options = {
          show_hidden = true,
        }
      })
    end
  },
  {
    "shellRaining/hlchunk.nvim",
    event = { "BufEnter" },
    config = function ()
      require('hlchunk').setup({
        blank = { enable = false },
        chunk = { enable = true },
        indent = { enable = false },
        line_num = { enable = false },
        context = { enable = false }
      })
    end
  },
  {
  "folke/flash.nvim",
    event = "VeryLazy",
    ---@type Flash.Config
    opts = {},
    -- stylua: ignore
    keys = {
      { "s", mode = { "n", "x", "o" }, function() require("flash").jump() end, desc = "Flash" },
      { "S", mode = { "n", "o", "x" }, function() require("flash").treesitter() end, desc = "Flash Treesitter" },
      { "r", mode = "o", function() require("flash").remote() end, desc = "Remote Flash" },
      { "R", mode = { "o", "x" }, function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
      { "<c-s>", mode = { "c" }, function() require("flash").toggle() end, desc = "Toggle Flash Search" },
    },
  },
  { -- Git and github related stuff
    'NeogitOrg/neogit',
    branch = 'nightly',
    event = 'VeryLazy',
    dependencies = {
      'nvim-lua/plenary.nvim',
      { 'sindrets/diffview.nvim', dependencies = 'nvim-lua/plenary.nvim' },
      { 'lewis6991/gitsigns.nvim', dependencies = { 'nvim-lua/plenary.nvim' } },
      { 'pwntester/octo.nvim',
        dependencies = {
          'nvim-lua/plenary.nvim',
          'nvim-telescope/telescope.nvim',
        },
      }
    },
    config = function ()
      local neogit = require('neogit')
      neogit.setup({
        use_magit_keybindings = true,
        integrations = {
          diffview = true
        },
      })
      require("diffview").setup({
        enhanced_diff_hl = true,
        use_icons = false
      })

      local gitsigns = require('gitsigns')
      gitsigns.setup {
        signcolumn = true,
        numhl      = true,
        linehl     = false,
        word_diff  = false,
        current_line_blame = false,
        current_line_blame_opts = {
          virt_text = true,
          virt_text_pos = 'right_align', -- 'eol' | 'overlay' | 'right_align'
          delay = 1000,
          ignore_whitespace = false,
        },
      }

      require"octo".setup({
        default_remote = {"upstream", "origin"}
      })
    end
  },
  {
    'adelarsq/neofsharp.vim',
    lazy = true,
    ft = "fsharp",
    config = function ()
      vim.fn.setenv("BUILDING_USING_DOTNET", "true")
    end
  },
  {
    'simrat39/rust-tools.nvim',
    lazy = true,
    ft = "rust",
    config = function ()
      require("rust-tools").setup({})
    end
  },
  -- IDE stuff pretty much, lsp, cmp, copilot, etc
  {
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
      'nvim-treesitter/nvim-treesitter-context'
    },
    build = ":TSUpdate",
    config = function ()
      local treesitter = require('nvim-treesitter.configs')
      local treesitter_context = require('treesitter-context')
      local treesitter_parsers = require('nvim-treesitter.parsers')
      local treesitter_highlighter = require('vim.treesitter.highlighter')

      vim.treesitter.language.register("markdown", "octo")
      treesitter_parsers.get_parser_configs().fsharp = {
        install_info = {
          url = "~/code/tree-sitter-fsharp",
          -- branch = "develop",
          files = {"src/scanner.c", "src/parser.c" }
        },
        filetype = "fsharp",
      }

      treesitter.setup {
        auto_install = true,
        ensure_installed = {
          "fsharp",
          "query",
          "lua", "rust", "c_sharp", "comment", "diff", "yaml",
          "git_rebase", "gitattributes", "gitcommit",
          "json", "markdown", "markdown_inline" },
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
          extended_mode = true,
          max_file_lines = nil,
        },
      }
    end
  },
  {
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v3.x',
    cmd = { 'LspInfo', 'Mason' },
    event = {'BufReadPre', 'BufNewFile'},
    dependencies = {
      {'neovim/nvim-lspconfig'},
      {
        'williamboman/mason.nvim',
        build = function()
          pcall(vim.cmd, 'MasonUpdate')
        end,
      },
      { 'WhoIsSethDaniel/mason-tool-installer.nvim' },
      { 'williamboman/mason-lspconfig.nvim' },
      { 'SmiteshP/nvim-navic' },
      { 'hrsh7th/nvim-cmp' },
      { 'dnlhc/glance.nvim' }
    },
    config = function()
      local lsp = require('lsp-zero').preset({})
      local lspconfig = require('lspconfig')
      require('mason').setup()

      require("mason-lspconfig").setup({
        ensure_installed = { "fsautocomplete", "rust_analyzer", "marksman", "prosemd_lsp", "grammarly" },
        handlers = { 
          lsp.default_setup
        }
      })

      require('mason-tool-installer').setup({
        ensure_installed = {
          'fantomas',
          'netcoredbg'
        },
        auto_update = true,
        run_on_start = true
      })

      lspconfig.fsautocomplete.setup({
        cmd = { 'fsautocomplete', '--adaptive-lsp-server-enabled' },
        root_dir = lspconfig.util.root_pattern('*.sln', '*.fsproj', '.git'),
        filetypes = { 'fsharp' },
        init_options = {
          AutomaticWorkspaceInit = true,
        }, 
        settings = {
          FSharp = {
            generateBinlog = "true",
            workspacePath = "FSharp.Compiler.Service.sln",
            excludeProjectDirectories = {
               ".git",
               "paket-files",
               ".paket",
               ".github",
               ".idea",
               "obj",
               "bin",
               "deploy",
               "dist",
               "node_modules",
               ".vscode",
               "vsintegration"
            },
            workspaceModePeekDeepLevel = 1, 
            enableAdaptiveLspServer = true,
            enableMSBuildProjectGraph = false,
            unusedDeclarationsAnalyzer = false,
            fsac = {
              cachedTypeCheckCount = 10000,
                gc = {
                  conserveMemory = 0,
                  heapCount = 10,
                  server = true,
                  noAffinitize = true
                },
                parallelReferenceResolution = true,
            },
            externalAutocomplete = true,
        },
        Fsharp = { fsac = { gc = { noAffinitize = true } } } 
       }
      })

      lsp.on_attach(
        function(client, bufnr)
          lsp.default_keymaps({buffer = bufnr})
          client.server_capabilities.semanticTokensProvider = true
          if client.server_capabilities.documentSymbolProvider then
            require('nvim-navic').attach(client, bufnr)
          end
          if client.server_capabilities.inlayHintProvider then
              vim.lsp.inlay_hint.enable(bufnr)
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

          if client.supports_method("textDocument/hover") then
            vim.g.cursorhold_updatetime = 1500
            vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
              buffer = bufnr,
              callback = function() hover_handler(client, bufnr) end
            })
          end
        end
      )

      lsp.set_server_config({
        capabilities = {
          textDocument = {
            foldingRange = {
              dynamicRegistration = false,
              lineFoldingOnly = true
            }
          }
        }
      })

      lsp.setup()
    end
  },
  {
      'hrsh7th/nvim-cmp',
      dependencies = {
        {'VonHeikemen/lsp-zero.nvim' },
        {'L3MON4D3/LuaSnip'},
        {'hrsh7th/cmp-nvim-lsp'},
        {'hrsh7th/cmp-nvim-lsp-document-symbol'},
        {'hrsh7th/cmp-nvim-lsp-signature-help'},
        {'hrsh7th/cmp-calc'},
        {'petertriho/cmp-git'},
        {'saadparwaiz1/cmp_luasnip'},
        {
          'zbirenbaum/copilot-cmp',
          dependencies = { 'zbirenbaum/copilot.lua' }
      }},
      config = function ()
        require('lsp-zero').extend_cmp()

        require("copilot").setup({
          panel = {
            enabled = false
          },
          suggestion = {
            enabled = false
          },
          filetypes = {
            yaml = false,
            markdown = true,
            gitcommit = true,
            gitrebase = false,
            ["."] = false,
          }
        })

        require('copilot_cmp').setup({
          method = "getCompletionsCycling",
          formatters = {
            label = require("copilot_cmp.format").format_label_text,
            insert_text = require("copilot_cmp.format").format_insert_text,
            preview = require("copilot_cmp.format").deindent,
          },
        })

        local luasnip = require('luasnip')
        require('luasnip.loaders.from_vscode').lazy_load()

        local cmp = require('cmp')
        cmp.setup({
          view = {
            entries = "custom",
            selection_order = 'near_cursor'
          },
          window = {
            completion = cmp.config.window.bordered(),
            documentation = cmp.config.window.bordered(),
          },
          sources = cmp.config.sources({
            { name = "copilot" },
            { name = 'nvim_lsp_document_symbol' },
            { name = 'nvim_lsp' },
            { name = 'nvim_lsp_signature_help' },
            { name = 'luasnip' },
            { name = 'git' }
          },{
            { name = 'buffer' },
            { name = 'calc' },
          }),
          mapping = {
            ['<CR>']      = cmp.mapping.confirm({
              behavior = cmp.ConfirmBehavior.Replace,
              select = false
            }),
            ['<C-p>']     = cmp.mapping.select_prev_item(),
            ['<C-n>']     = cmp.mapping.select_next_item(),
            ['<Up>']      = cmp.mapping.select_prev_item(),
            ['<Down>']    = cmp.mapping.select_next_item(),
            ['<C-d>']     = cmp.mapping.scroll_docs(-4),
            ['<C-f>']     = cmp.mapping.scroll_docs(4),
            ['<C-Space>'] = cmp.mapping.complete(),
            ['<C-e>']     = cmp.mapping.close(),
            ['<Tab>']     = function(fallback)
              if cmp.visible() and has_words_before() then
                cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
              elseif luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
              else
                fallback()
              end
            end,
            ['<S-Tab>'] = function(fallback)
              if cmp.visible() and has_words_before() then
                cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
              elseif luasnip.jumpable(-1) then
                luasnip.jump(-1)
              else
                fallback()
              end
            end,
          }
        })

        cmp.setup.filetype('gitcommit', {
          sources = cmp.config.sources({
            { name = 'cmp_git' },
          }, {
            { name = 'buffer' },
          })
        })

        cmp.setup.cmdline({ '/', '?' }, {
          mapping = cmp.mapping.preset.cmdline(),
          sources = cmp.config.sources({
            { name = 'nvim_lsp_document_symbol' }
          }, {
            { name = 'buffer' }
          }),
          view = { entries = "native" },
        })

        cmp.event:on("menu_opened", function ()
          vim.b.copilot_suggestion_hidden = true
        end)

        cmp.event:on("menu_closed", function ()
          vim.b.copilot_suggestion_hidden = false
        end)

        local format = require("cmp_git.format")
        local sort = require("cmp_git.sort")
        require("cmp_git").setup({
            -- defaults
            filetypes = { "gitcommit", "octo" },
            remotes = { "upstream", "origin" }, -- in order of most to least prioritized
            enableRemoteUrlRewrites = false, -- enable git url rewrites, see https://git-scm.com/docs/git-config#Documentation/git-config.txt-urlltbasegtinsteadOf
            git = {
                commits = {
                    limit = 100,
                    sort_by = sort.git.commits,
                    format = format.git.commits,
                },
            },
            github = {
                issues = {
                    fields = { "title", "number", "body", "updatedAt", "state" },
                    filter = "all", -- assigned, created, mentioned, subscribed, all, repos
                    limit = 100,
                    state = "open", -- open, closed, all
                    sort_by = sort.github.issues,
                    format = format.github.issues,
                },
                mentions = {
                    limit = 100,
                    sort_by = sort.github.mentions,
                    format = format.github.mentions,
                },
                pull_requests = {
                    fields = { "title", "number", "body", "updatedAt", "state" },
                    limit = 100,
                    state = "open", -- open, closed, merged, all
                    sort_by = sort.github.pull_requests,
                    format = format.github.pull_requests,
                },
            },
            trigger_actions = {
                {
                    debug_name = "git_commits",
                    trigger_character = ":",
                    action = function(sources, trigger_char, callback, params, git_info)
                        return sources.git:get_commits(callback, params, trigger_char)
                    end,
                },
                {
                    debug_name = "github_issues_and_pr",
                    trigger_character = "#",
                    action = function(sources, trigger_char, callback, params, git_info)
                        return sources.github:get_issues_and_prs(callback, git_info, trigger_char)
                    end,
                },
                {
                    debug_name = "github_mentions",
                    trigger_character = "@",
                    action = function(sources, trigger_char, callback, params, git_info)
                        return sources.github:get_mentions(callback, git_info, trigger_char)
                    end,
                },
            },
          }
        )
      end
  },
  {
    'RRethy/vim-illuminate',
    event = { 'BufEnter' },
    config = function ()
      require('illuminate').configure({
        providers = {
              'lsp',
              'treesitter',
              'regex',
          },
        delay = 50
      })
    end
  },
  {
    'kevinhwang91/nvim-ufo',
    dependencies = {
      'kevinhwang91/promise-async',
      'nvim-treesitter/nvim-treesitter'
    },
    config = function ()
      local treesitter_highlighter = require('vim.treesitter.highlighter')
      local ufo_virt_text_handler = function(virtText, lnum, endLnum, width, truncate)
          local newVirtText = {}
          local suffix = ('  %d '):format(endLnum - lnum)
          local sufWidth = vim.fn.strdisplaywidth(suffix)
          local targetWidth = width - sufWidth
          local curWidth = 0
          for _, chunk in ipairs(virtText) do
              local chunkText = chunk[1]
              local chunkWidth = vim.fn.strdisplaywidth(chunkText)
              if targetWidth > curWidth + chunkWidth then
                  table.insert(newVirtText, chunk)
              else
                  chunkText = truncate(chunkText, targetWidth - curWidth)
                  local hlGroup = chunk[2]
                  table.insert(newVirtText, {chunkText, hlGroup})
                  chunkWidth = vim.fn.strdisplaywidth(chunkText)
                  -- str width returned from truncate() may less than 2nd argument, need padding
                  if curWidth + chunkWidth < targetWidth then
                      suffix = suffix .. (' '):rep(targetWidth - curWidth - chunkWidth)
                  end
                  break
              end
              curWidth = curWidth + chunkWidth
          end
          table.insert(newVirtText, {suffix, 'MoreMsg'})
          return newVirtText
      end
      require('ufo').setup({
        open_fold_hl_timeout = 150,
        close_fold_kinds_ft = {'imports', 'comment', 'region'},
        preview = {
            win_config = {
                border = {'', '─', '', '', '', '─', '', ''},
                winhighlight = 'Normal:Folded',
                winblend = 0
            },
            mappings = {
                scrollU = '<C-u>',
                scrollD = '<C-d>'
            }
        },
        fold_virt_text_handler = ufo_virt_text_handler,
        provider_selector = function(bufnr, _, _)
              if treesitter_highlighter.active[bufnr] then
                return {'treesitter', 'indent'}
              else
                return {'lsp', 'indent'}
              end
          end
      })
      vim.keymap.set('n', 'zR', require('ufo').openAllFolds)
      vim.keymap.set('n', 'zM', require('ufo').closeAllFolds)
      vim.keymap.set('n', 'zr', require('ufo').openFoldsExceptKinds)
      vim.keymap.set('n', 'zm', require('ufo').closeFoldsWith)
    end
  },
  {
    'stevearc/aerial.nvim',
    cond = false, -- Disable, it's not that useful as for now.
    config = function ()
      local aerial = require('aerial')
      aerial.setup({
        on_attach = function(bufnr)
          aerial.open_all()
        end,
        backends = { "treesitter", "lsp", "markdown", "man" },
        close_automatic_events = { "unfocus", "switch_buffer", "unsupported" },
        open_automatic = true,
        show_guides = true,
        attach_mode = "window",
        layout = {
          max_width = { 80, 0.25 },
          width = nil,
          min_width = 30,
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
    end
  },
  {
    'dgagn/diagflow.nvim',
    cond = true,
    opts = {},
    config = function ()
      require('diagflow').setup({
          enable = true,
          max_width = 60,  -- The maximum width of the diagnostic messages
          severity_colors = {  -- The highlight groups to use for each diagnostic severity level
              error = "DiagnosticFloatingError",
              warning = "DiagnosticFloatingWarn",
              info = "DiagnosticFloatingInfo",
              hint = "DiagnosticFloatingHint",
          },
          gap_size = 1,
          scope = 'cursor', -- 'cursor', 'line'
          padding_top = 0,
          padding_right = 0,
          text_align = 'right', -- 'left', 'right'
          placement = 'top', -- 'top', 'inline'
          inline_padding_left = 0, -- the padding left when the placement is inline
          update_event = { 'DiagnosticChanged' }, -- the event that updates the diagnostics cache
          toggle_event = { }, -- if InsertEnter, can toggle the diagnostics on inserts
          show_sign = false, -- set to true if you want to render the diagnostic sign before the diagnostic message
      })
    end
  },
  {
      'j-hui/fidget.nvim',
      event = 'LspAttach',
      branch = 'legacy',
      config = function ()
        local fidget = require('fidget')

        fidget.setup({
          text = {
            done = "done:",
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
      end
  },
  {
    'rmagatti/goto-preview',
    event = 'LspAttach',
    config = function()
      require('goto-preview').setup {}
    end
  },
  {
    'dnlhc/glance.nvim',
    event = 'LspAttach',
    config = function ()
      local glance = require('glance')
      local glance_actions = glance.actions
      glance.setup({
        height = 18,
        zindex = 45,
        preview_win_opts = {
          cursorline = true,
          number = true,
          wrap = true,
        },
        border = {
          enable = true,
          top_char = '―',
          bottom_char = '―',
        },
        list = {
          position = 'right', -- Position of the list window 'left'|'right'
          width = 0.33, -- 33% width relative to the active window, min 0.1, max 0.5
        },
        theme = { -- This feature might not work properly in nvim-0.7.2
          enable = false, -- Will generate colors for the plugin based on your current colorscheme
          mode = 'auto', -- 'brighten'|'darken'|'auto', 'auto' will set mode based on the brightness of your colorscheme
        },
        mappings = {
          list = {
            ['j'] = glance_actions.next, -- Bring the cursor to the next item in the list
            ['k'] = glance_actions.previous, -- Bring the cursor to the previous item in the list
            ['<Down>'] = glance_actions.next,
            ['<Up>'] = glance_actions.previous,
            ['<Tab>'] = glance_actions.next_location, -- Bring the cursor to the next location skipping groups in the list
            ['<S-Tab>'] = glance_actions.previous_location, -- Bring the cursor to the previous location skipping groups in the list
            ['<C-u>'] = glance_actions.preview_scroll_win(5),
            ['<C-d>'] = glance_actions.preview_scroll_win(-5),
            ['v'] = glance_actions.jump_vsplit,
            ['s'] = glance_actions.jump_split,
            ['t'] = glance_actions.jump_tab,
            ['<CR>'] = glance_actions.jump,
            ['o'] = glance_actions.jump,
            ['<leader>l'] = glance_actions.enter_win('preview'), -- Focus preview window
            ['q'] = glance_actions.close,
            ['Q'] = glance_actions.close,
            ['<Esc>'] = glance_actions.close,
            -- ['<Esc>'] = false -- disable a mapping
          },
          preview = {
            ['Q'] = glance_actions.close,
            ['<Tab>'] = glance_actions.next_location,
            ['<S-Tab>'] = glance_actions.previous_location,
            ['<leader>l'] = glance_actions.enter_win('list'), -- Focus list window
          },
        },
        hooks = {
          before_open = function(results, open, jump, method)
            if #results == 1 then
              jump(results[1]) -- argument is optional
            else
              open(results) -- argument is optional
            end
          end,
        },
        folds = {
          fold_closed = '+',
          fold_open = '-',
          folded = true,
        },
        indent_lines = {
          enable = false,
          icon = '│',
        },
        winbar = {
          enable = true,
        },
      })
    end
  }
},
{
  root = vim.fn.stdpath("data") .. "/lazy",
  defaults = {
    lazy = true, -- should plugins be lazy-loaded?
    version = nil,
    cond = nil,
  },
  spec = nil, ---@type LazySpec
  lockfile = vim.fn.stdpath("config") .. "/lazy-lock.json", -- lockfile generated after running update.
  concurrency = jit.os:find("Windows") and (vim.loop.available_parallelism() * 2) or nil, ---@type number limit the maximum amount of concurrent tasks
  git = {
    log = { "-8" }, -- show commits from the last 3 days
    timeout = 120, -- kill processes that take more than 2 minutes
    url_format = "https://github.com/%s.git",
    filter = true,
  },
  install = {
    missing = true,
  },
  ui = {
    pills = true,
    icons = {
      cmd = "[cmd] ",
      config = "[config]",
      event = "[event]",
      ft = "[filetype] ",
      init = "[init] ",
      import = "[import] ",
      keys = "[keys] ",
      lazy = "[lazy] ",
      loaded = "[loaded]",
      not_loaded = "[not loaded]",
      plugin = "[plugin] ",
      runtime = "[runtime] ",
      source = "[source] ",
      start = "[start]",
      task = "[task] ",
      list = {
        "+",
        "-",
        "*",
        "--",
      },
    },
  },
  diff = {
    cmd = "git",
  },
  checker = {
    enabled = true,
    concurrency = nil,
    notify = true,
    frequency = 3600,
  },
  change_detection = {
    enabled = true,
    notify = true,
  }
})
