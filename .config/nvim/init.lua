local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
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

vim.o.termguicolors = true
vim.o.cursorline = true
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

vim.cmd [[ set clipboard+=unnamedplus ]]

vim.api.nvim_command('autocmd BufNewFile,BufRead *.fs,*.fsx,*.fsi,*.fsl,*.fsy set filetype=fsharp')
vim.api.nvim_command('autocmd BufNewFile,BufRead *.fsproj,*.csproj,*.vbproj,*.cproj,*.proj set filetype=xml')

local has_words_before = function()
  if vim.api.nvim_buf_get_option(0, "buftype") == "prompt" then return false end
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_text(0, line-1, 0, line-1, col, {})[1]:match("^%s*$") == nil
end

vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
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
    vim.keymap.set('n', '<space>f', function()
      vim.lsp.buf.format { async = true }
    end, opts)
  end,
})

vim.diagnostic.config({
    virtual_text = false,
    virtual_lines = { only_current_line = true }
})

function OpenDiagnosticIfNoFloat()
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

vim.api.nvim_create_augroup("lsp_diagnostics_hold", { clear = true })
vim.api.nvim_create_autocmd({ "CursorHold" }, {
  pattern = "*",
  command = "lua OpenDiagnosticIfNoFloat()",
  group = "lsp_diagnostics_hold",
})

require("lazy").setup({
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      local tokyonight = require("tokyonight")
      tokyonight.setup({
        style = "day",
        light_style = "day",
        styles = {
          comments = { italic = false },
          keywords = { italic = false },
          functions = {},
          variables = {},
          sidebars = "dark",
          floats = "dark",
        }
      })
      vim.cmd([[colorscheme tokyonight]])
    end,
  },
  {
    'hoob3rt/lualine.nvim',
    lazy = false,
    config = function ()
      require('lualine').setup {
        options = {
          theme = 'tokyonight'
        }
      }
    end
  },
  {
    'nvim-lua/plenary.nvim',
    lazy = true
  },
  {
    'gelguy/wilder.nvim',
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
        integrations = {
          diffview = true
        },
      })
      require("diffview").setup({
        enhanced_diff_hl = true
      })

      local gitsigns = require('gitsigns')
      gitsigns.setup {
        signcolumn = true,
        numhl      = true,
        linehl     = true,
        word_diff  = true,
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
    ft = "fsharp"
  },
  { -- IDE stuff pretty much, lsp, cmp, copilot, etc
    'VonHeikemen/lsp-zero.nvim',
    branch = 'dev-v3',
    dependencies = {
      {'neovim/nvim-lspconfig'},
      {
        'williamboman/mason.nvim',
        build = function()
          pcall(vim.cmd, 'MasonUpdate')
        end,
      },
      { 'williamboman/mason-lspconfig.nvim' },
      { 'williamboman/mason-null-ls.nvim',
        event = { "BufReadPre", "BufNewFile" },
        dependencies = {
          "williamboman/mason.nvim",
          {
            "jose-elias-alvarez/null-ls.nvim",
            dependencies = {
              'nvim-lua/plenary.nvim'
            }
          },
        }
      },
      {'lvimuser/lsp-inlayhints.nvim'},
      {'SmiteshP/nvim-navic'},
      {'L3MON4D3/LuaSnip'},
      {'hrsh7th/nvim-cmp'},
      {'hrsh7th/cmp-nvim-lsp'},
      {'hrsh7th/cmp-nvim-lsp-document-symbol'},
      {'hrsh7th/cmp-nvim-lsp-signature-help'},
      {'hrsh7th/cmp-calc'},
      {'petertriho/cmp-git'},
      {'saadparwaiz1/cmp_luasnip'},
      {
        'zbirenbaum/copilot-cmp',
        dependencies = { 'zbirenbaum/copilot.lua' }
      },
      {
        'nvim-treesitter/nvim-treesitter',
        dependencies = {
          'nvim-treesitter/playground',
          'nvim-treesitter/nvim-treesitter-context'
        },
        build = function()
          pcall(vim.cmd, 'TSUpdate')
        end,
      }

    },
    config = function()
      local lsp = require('lsp-zero').preset({})
      local ih = require("lsp-inlayhints")
      local cmp = require('cmp')
      ih.setup({
        inlay_hints = {
          parameter_hints = {
            show = true,
            prefix = "<- ",
            separator = ", ",
            remove_colon_start = true,
            remove_colon_end = true,
          },
          type_hints = {
            -- type and other hints
            show = true,
            prefix = "",
            separator = ", ",
            remove_colon_start = true,
            remove_colon_end = true,
          },
        },
        enabled_at_startup = true,
      })
      require('mason').setup()
      require("mason-lspconfig").setup({
        ensure_installed = { "fsautocomplete" },
        handlers = {lsp.default_setup}
      })

      lsp.on_attach(function(client, bufnr)
        ih.on_attach(client, bufnr, false)
        lsp.default_keymaps({buffer = bufnr})
        if client.server_capabilities.documentSymbolProvider then
          require('nvim-navic').attach(client, bufnr)
        end
        if client.server_capabilities.inlayHintProvider then
            --vim.lsp.buf.inlay_hint(bufnr, true)
        end
      end)

      lsp.setup()
      lsp.extend_cmp()

      local cmp_action = lsp.cmp_action()

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
          help = false,
          gitcommit = false,
          gitrebase = false,
          hgcommit = false,
          svn = false,
          cvs = false,
          ["."] = false,
        },
        copilot_node_command = 'node',
        server_opts_overrides = {},
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
      cmp.setup({
        view = {
          entries = "custom",
          selection_order = 'near_cursor'
        },
        sources = cmp.config.sources({
          { name = "copilot" },
          -- { name = 'nvim_lsp_signature_help' },
          { name = 'nvim_lsp' },
          { name = 'luasnip' },
          { name = 'git' }
        },{
          { name = 'buffer' },
          { name = 'calc' },
          -- { name = "crates" },
        }),
        mapping = {
          ['<CR>']      = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Replace,
            select = false
          }),
          ['<C -p>']     = cmp.mapping.select_prev_item(),
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

      local null_ls = require('null-ls')
      null_ls.setup({
        sources = {
          null_ls.builtins.formatting.fantomas,
          null_ls.builtins.diagnostics.gitlint,
          null_ls.builtins.code_actions.gitrebase,
          null_ls.builtins.code_actions.gitsigns,
          null_ls.builtins.completion.luasnip,
          null_ls.builtins.completion.tags,
          null_ls.builtins.hover.dictionary,
          null_ls.builtins.hover.printenv
        }
      })
      require('mason-null-ls').setup({
        ensure_installed = { 'fantomas' },
        automatic_installation = true,
        handlers = {}
      })

      local treesitter = require('nvim-treesitter.configs')
      local treesitter_context = require('treesitter-context')
      local treesitter_parsers = require('nvim-treesitter.parsers')
      local treesitter_highlighter = require('vim.treesitter.highlighter')

      vim.treesitter.language.register("markdown", "octo")

      treesitter.setup {
        ensure_installed = {
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
      'j-hui/fidget.nvim',
      event = 'LspAttach',
      branch = 'legacy',
      opts = { window = { blend = 0 } },
  },
})
