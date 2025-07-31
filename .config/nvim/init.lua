vim.g.mapleader = " "

vim.opt.completeopt = { 'fuzzy', 'menu', 'menuone', 'noselect', 'popup' }
vim.opt.foldlevel = 99
vim.opt.foldlevelstart = 99
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 2
vim.opt.showtabline = 0
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

vim.opt.foldenable = true
vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
vim.opt.foldcolumn = "1"
vim.opt.foldlevel = 99
vim.opt.foldlevelstart = 99
vim.opt.foldenable = true

if vim.g.neovide then
  vim.g.neovide_position_animation_length = 0
  vim.g.neovide_cursor_animation_length = 0.00
  vim.g.neovide_cursor_trail_size = 0
  vim.g.neovide_cursor_animate_in_insert_mode = false
  vim.g.neovide_cursor_animate_command_line = false
  vim.g.neovide_scroll_animation_far_lines = 0
  vim.g.neovide_scroll_animation_length = 0.00
  vim.g.neovide_padding_top = 0
  vim.g.neovide_padding_bottom = 0
  vim.g.neovide_padding_right = 0
  vim.g.neovide_padding_left = 0
  vim.g.neovide_opacity = 0.5
  vim.g.neovide_window_blurred = true
  vim.g.neovide_title_background_color = string.format(
    "%x",
    vim.api.nvim_get_hl(0, {id=vim.api.nvim_get_hl_id_by_name("Normal")}).bg
  )
  vim.g.neovide_show_border = true
  vim.g.neovide_theme = 'auto'
  vim.g.neovide_refresh_rate = 60
  vim.g.experimental_layer_grouping = true
  vim.g.neovide_refresh_rate_idle = 1
  vim.g.neovide_fullscreen = false
  vim.g.neovide_macos_simple_fullscreen = true
end

vim.cmd.cabbrev('git', 'Neogit')
vim.cmd.cabbrev('Git', 'Neogit')

vim.api.nvim_create_user_command(
  'PackUpdate',
  "lua vim.pack.update()",
  {bang = true, desc = "Update NVIM plugins"}
)

vim.api.nvim_create_user_command(
  'Git',
  'Neogit',
  { bang = true, desc = "Alias to Neogit" }
)

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

vim.pack.add(
  {
    { src = 'https://github.com/vhyrro/luarocks.nvim' },
    { src = 'https://github.com/echasnovski/mini.nvim' },
    { src = 'https://github.com/echasnovski/mini.pick' },
    { src = 'https://github.com/nvim-lua/plenary.nvim' },
    { src = 'https://github.com/nvim-telescope/telescope.nvim' },

    { src = 'https://github.com/MunifTanjim/nui.nvim' },
    { src = 'https://github.com/rcarriga/nvim-notify' },
    { src = 'https://github.com/folke/noice.nvim' },

    { src = 'https://github.com/stevearc/oil.nvim' },

    { src = 'https://github.com/tkancf/narrowing-nvim' },

    { src = 'https://github.com/amitds1997/remote-nvim.nvim' },

    { src = 'https://github.com/folke/tokyonight.nvim' },
    { src = 'https://github.com/f-person/auto-dark-mode.nvim' },
    { src = 'https://github.com/nvim-tree/nvim-web-devicons' },

    { src = 'https://github.com/mason-org/mason.nvim' },
    { src = 'https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim' },

    { src = 'https://github.com/nvim-treesitter/nvim-treesitter' },
    { src = 'https://github.com/nvim-treesitter/nvim-treesitter-context' },

    { src = 'https://github.com/MeanderingProgrammer/render-markdown.nvim' },

    { src = 'https://github.com/onsails/lspkind.nvim' },
    { src = 'https://github.com/jinzhongjia/LspUI.nvim', version = 'main' },

    { src = 'https://github.com/zbirenbaum/copilot.lua' },
    { src = 'https://github.com/yetone/avante.nvim' },

    { src = 'https://github.com/saghen/blink.cmp', version = 'main' },
    { src = 'https://github.com/fang2hou/blink-copilot' },

    { src = 'https://github.com/j-hui/fidget.nvim' },
    { src = 'https://github.com/Bekaboo/dropbar.nvim' },

    { src = 'https://github.com/y3owk1n/time-machine.nvim' },

    { src = 'https://github.com/sindrets/diffview.nvim' },
    { src = 'https://github.com/lewis6991/gitsigns.nvim' },
    { src = 'https://github.com/pwntester/octo.nvim' },
    { src = 'https://github.com/NeogitOrg/neogit' },

    { src = 'https://github.com/chrisgrieser/nvim-origami' },

    { src = 'https://github.com/shortcuts/no-neck-pain.nvim' },

    { src = 'https://github.com/code-biscuits/nvim-biscuits' },

    { src = 'https://github.com/dgagn/diagflow.nvim' }
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

require('nvim-web-devicons').setup({
  default = true
})

require("oil").setup()

require('narrowing').setup({keymaps = { enabled = true }})

require('remote-nvim').setup({})

vim.keymap.set('i', '<c-space>', vim.lsp.completion.get)

vim.diagnostic.config({
  virtual_text = false, --{ current_line = false },
  update_in_insert = true,
  underline = true,
  severity_sort = true,
})

local treesitter_configs = { 'c', 'cpp', 'rust', 'yaml', 'markdown', 'latex', 'html', 'typescript', 'javascript', 'regex', 'bash', 'lua' }
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
  },
  ['lua-language-server'] = {
    cmd = { 'lua-language-server' },
    root_markers = { '.luarc.json', '.luarc.jsonc', '.luacheckrc', 'init.lua', 'init.json', 'init.jsonc', '.git', '*.lua' },
    filetypes = { 'lua' },
    single_file_support = true,
    settings = {
      Lua = {
        runtime = {
          version = 'LuaJIT',
          path = vim.split(package.path, ';'),
        },
        diagnostics = {
          globals = { 'vim' },
        },
        workspace = {
          library = vim.api.nvim_get_runtime_file("", true),
          checkThirdParty = false,
        },
        telemetry = {
          enable = false,
        },
      },
    },
  }
}

local tools = { 'clang-format', 'codelldb', 'copilot-language-server' }

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

require("LspUI").setup({
  prompt = {
    border = true,
    borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
  },
  code_action = {
    enable = true,
    command_enable = true,
    gitsigns = true,
    extend_gitsigns = true,
    ui = {
      title = "Code Action",
      border = "rounded",
      winblend = 0,
    },
    keys = {
      quit = "q",
      exec = "<CR>",
    },
  },
  hover = {
    enable = true,
    command_enable = true,
    ui = {
      title = "Hover",
      border = "rounded",
      winblend = 0,
    },
    keys = {
      quit = "q",
    },
  },
  rename = {
    enable = true,
    command_enable = true,
    auto_save = false,
    ui = {
      title = "Rename",
      border = "rounded",
      winblend = 0,
    },
    keys = {
      quit = "<C-c>",
      exec = "<CR>",
    },
  },
  diagnostic = {
    enable = true,
    command_enable = true,
    ui = {
      title = "Diagnostic",
      border = "rounded",
      winblend = 0,
    },
    keys = {
      quit = "q",
      exec = "<CR>",
    },
  },
  definition = {
    enable = true,
    command_enable = true,
    ui = {
      title = "Definition",
      border = "rounded",
      winblend = 0,
    },
    keys = {
      quit = "q",
      exec = "<CR>",
      vsplit = "v",
      split = "s",
      tabe = "t",
    },
  },
  reference = {
    enable = true,
    command_enable = true,
    ui = {
      title = "Reference",
      border = "rounded",
      winblend = 0,
    },
    keys = {
      quit = "q",
      exec = "<CR>",
      vsplit = "v",
      split = "s",
      tabe = "t",
    },
  },
  implementation = {
    enable = true,
    command_enable = true,
    ui = {
      title = "Implementation",
      border = "rounded",
      winblend = 0,
    },
    keys = {
      quit = "q",
      exec = "<CR>",
      vsplit = "v",
      split = "s",
      tabe = "t",
    },
  },
  type_definition = {
    enable = true,
    command_enable = true,
    ui = {
      title = "Type Definition",
      border = "rounded",
      winblend = 0,
    },
    keys = {
      quit = "q",
      exec = "<CR>",
      vsplit = "v",
      split = "s",
      tabe = "t",
    },
  },
  declaration = {
    enable = true,
    command_enable = true,
    ui = {
      title = "Declaration",
      border = "rounded",
      winblend = 0,
    },
    keys = {
      quit = "q",
      exec = "<CR>",
      vsplit = "v",
      split = "s",
      tabe = "t",
    },
  },
  call_hierarchy = {
    enable = true,
    command_enable = true,
    ui = {
      title = "Call Hierarchy",
      border = "rounded",
      winblend = 0,
    },
    keys = {
      quit = "q",
      exec = "<CR>",
      expand = "o",
      jump = "e",
      vsplit = "v",
      split = "s",
      tabe = "t",
    },
  },
  lightbulb = {
    enable = true,
    command_enable = true,
    icon = "→",
    action_kind = {
      QuickFix = "⚒",
      Refactor = "⟲",
      RefactorExtract = "⤴",
      RefactorInline = "⤵",
      RefactorRewrite = "✎",
      Source = "⚑",
      SourceOrganizeImports = "⚙",
    },
  },
  inlay_hint = {
    enable = false,
    command_enable = true,
  },
  signature = {
    enable = true,
    command_enable = true,
    ui = {
      title = "Signature Help",
      border = "rounded",
      winblend = 0,
    },
    keys = {
      quit = "q",
    },
  },
})
vim.keymap.set("n", "K", "<cmd>LspUI hover<CR>")
vim.keymap.set("n", "gr", "<cmd>LspUI reference<CR>")
vim.keymap.set("n", "gd", "<cmd>LspUI definition<CR>")
vim.keymap.set("n", "gt", "<cmd>LspUI type_definition<CR>")
vim.keymap.set("n", "gi", "<cmd>LspUI implementation<CR>")
vim.keymap.set("n", "<leader>rn", "<cmd>LspUI rename<CR>")
vim.keymap.set("n", "<leader>ca", "<cmd>LspUI code_action<CR>")
vim.keymap.set("n", "<leader>ci", "<cmd>LspUI call_hierarchy incoming_calls<CR>")
vim.keymap.set("n", "<leader>co", "<cmd>LspUI call_hierarchy outgoing_calls<CR>")

vim.treesitter.language.register('markdown', 'octo')

require('nvim-treesitter.install').prefer_git = true
require('nvim-treesitter.configs').setup({
  ensure_installed = treesitter_configs,
  ignore_install = {},
  auto_install = true,
  sync_install = false,
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
})
require('treesitter-context').setup({enable = true})

require('mason').setup()
local all_tools = merge_table(vim.tbl_keys(lsp_configs), tools)
for _, tool in pairs(all_tools) do
    if vim.fn.executable(tool) == 0 then
        print("Installing tool: " .. tool)
        vim.cmd("MasonInstall " .. tool)
    end
end

vim.lsp.enable(vim.tbl_keys(lsp_configs))

if vim.g.lsp_on_demands then
  vim.lsp.enable(vim.g.lsp_on_demands)
end

require('render-markdown').setup({
  enabled = true,
  file_types = { "markdown", "octo", "quarto", "Avante" },
  completions = { blink = { enabled = true } },
  render_modes = true,
  anti_conceal = {
        enabled = true
  },
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
    ['<Up>'] = { 'select_prev', 'fallback' },
    ['<Down>'] = { 'select_next', 'fallback' },
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
        components = {
          kind_icon = {
            text = function(ctx)
              local icon = ctx.kind_icon
              if vim.tbl_contains({ "Path" }, ctx.source_name) then
                  local dev_icon, _ = require("nvim-web-devicons").get_icon(ctx.label)
                  if dev_icon then
                      icon = dev_icon
                  end
              else
                  icon = require("lspkind").symbolic(ctx.kind, {
                      mode = "symbol",
                  })
              end

              return icon .. ctx.icon_gap
            end,

            -- Optionally, use the highlight groups from nvim-web-devicons
            -- You can also add the same function for `kind.highlight` if you want to
            -- keep the highlight groups in sync with the icons.
            highlight = function(ctx)
              local hl = ctx.kind_hl
              if vim.tbl_contains({ "Path" }, ctx.source_name) then
                local dev_icon, dev_hl = require("nvim-web-devicons").get_icon(ctx.label)
                if dev_icon then
                  hl = dev_hl
                end
              end
              return hl
            end,
          }
        },
        columns = {
            --{ "kind_icon" },
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
  sources = {
    default = { 'copilot', 'lsp', 'path', 'buffer' },
    providers = {
      copilot = {
        name = "copilot",
        module = "blink-copilot",
        score_offset = 100,
        async = true,
      },
    },
  },
  cmdline = {
    keymap = {
      ['<Up>'] = { 'select_prev', 'fallback' },
      ['<Down>'] = { 'select_next', 'fallback' },
      ['<Right>'] = { 'accept', 'fallback' },
      ['<Esc>'] = { 'hide', 'fallback' },
    },
    completion = { menu = { auto_show = false }, ghost_text = { enabled = true } },
  },
  fuzzy = {
    implementation = 'prefer_rust',
    prebuilt_binaries = {
      download = true,
      ignore_version_mismatch = true,
    },
    sorts = {
      'exact',
      'score',
      'sort_text',
    },
  }
})

require('blink-copilot').setup({})

require("copilot").setup({
  suggestion = { enabled = false },
  panel = { enabled = false },
  filetypes = {
    cmdline = false,
    ["*"] = true,
  },
})

require('avante').setup({
  mode = 'agentic',
  provider = 'copilot',
  cursor_applying_provider = 'copilot',
  auto_suggestions_provider = 'copilot',
  providers = {
    copilot = {
      model = "claude-sonnet-4",
    }
  },
  behaviour = {
    auto_suggestions = false,
    auto_set_highlight_group = true,
    auto_set_keymaps = true,
    auto_apply_diff_after_generation = false,
    support_paste_from_clipboard = false,
    minimize_diff = true,
    enable_token_counting = true,
    auto_approve_tool_permissions = false,
    enable_cursor_planning_mode = true,
  },
  hints = { enabled = true },
})

require("fidget").setup({})
require('dropbar').setup({
  icons = {
    enable = true,
    ui = {
      bar = {
        separator = ' → ',
        extends = '…',
      }
    },
    kinds = {
      symbols = {
        Array = '[] ',
        BlockMappingPair = '{} ',
        Boolean = 'ß ',
        BreakStatement = '↵ ',
        Call = '⟐ ',
        CaseStatement = '⇒ ',
        Class = 'C ',
        Color = '# ',
        Constant = 'K ',
        Constructor = '⊕ ',
        ContinueStatement = '→ ',
        Copilot = '© ',
        Declaration = 'D ',
        Delete = '✗ ',
        DoStatement = '∞ ',
        Element = '⊙ ',
        Enum = 'E ',
        EnumMember = 'E ',
        Event = '! ',
        Field = '⌘ ',
        File = 'f ',
        Folder = '/ ',
        ForStatement = '∀ ',
        Function = 'λ ',
        GotoStatement = '↪ ',
        Identifier = 'α ',
        IfStatement = '? ',
        Interface = 'I ',
        Keyword = '$ ',
        List = '≡ ',
        Log = '¶ ',
        Lsp = '⚙ ',
        Macro = 'μ ',
        MarkdownH1 = '1 ',
        MarkdownH2 = '2 ',
        MarkdownH3 = '3 ',
        MarkdownH4 = '4 ',
        MarkdownH5 = '5 ',
        MarkdownH6 = '6 ',
        Method = 'ƒ ',
        Module = '⊞ ',
        Namespace = '⋯ ',
        Null = '∅ ',
        Number = '# ',
        Object = '○ ',
        Operator = '± ',
        Package = 'P ',
        Pair = '⇔ ',
        Property = '· ',
        Reference = '& ',
        Regex = '/ ',
        Repeat = '∞ ',
        Return = '← ',
        RuleSet = '§ ',
        Scope = '⊃ ',
        Section = '¶ ',
        Snippet = '* ',
        Specifier = '@ ',
        Statement = '> ',
        String = '" ',
        Struct = 'S ',
        SwitchStatement = '⤷ ',
        Table = '⊞ ',
        Terminal = '>_ ',
        Text = 'T ',
        Type = '𝜏 ',
        TypeParameter = '⟨⟩ ',
        Unit = '° ',
        Value = 'V ',
        Variable = 'x ',
        WhileStatement = '⥁ ',
      }
    }
  }
})

require("time-machine").setup({})
require('diffview').setup({
  enhanced_diff_hl = true,
  use_icons = false,
  show_help_hints = true,
  watch_index = true,
  view = {
    default = {
      layout = 'diff2_horizontal'
    },
    merge_tool = {
      layout = 'diff4_mixed',
      disable_diagnostics = true,
      winbar_info = true
    }
  }
})
require('gitsigns').setup {
  signs_staged_enable = true,
  signcolumn = true,
  numhl      = true,
  linehl     = true,
  word_diff  = true,
  watch_gitdir = {
    follow_files = true
  },
  auto_attach = true,
  attach_to_untracked = false,
  preview_config = {
    style = 'minimal',
    relative = 'cursor',
    row = 0,
    col = 1
  },
}
require('octo').setup({})
require('neogit').setup({
  graph_style = 'unicode',
  process_spinner = true,
  highlight = {
    italic = false,
    bold = true,
    underline = true
  },
  initial_branch_name = 'main',
})

require("noice").setup({
  lsp = {
    override = {
      ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
      ["vim.lsp.util.stylize_markdown"] = true,
    },
  },
  presets = {
    bottom_search = true,
    command_palette = true,
    long_message_to_split = true,
  },
})

require("no-neck-pain").setup({})

require('nvim-biscuits').setup({})

require('diagflow').setup({
    enable = true,
    max_width = 60,
    max_height = 10,
    severity_colors = {
        error = "DiagnosticFloatingError",
        warning = "DiagnosticFloatingWarn",
        info = "DiagnosticFloatingInfo",
        hint = "DiagnosticFloatingHint",
    },
    format = function(diagnostic)
      return diagnostic.message
    end,
    gap_size = 1,
    scope = 'line', -- 'cursor', 'line' this changes the scope, so instead of showing errors under the cursor, it shows errors on the entire line.
    padding_top = 0,
    padding_right = 0,
    text_align = 'right',
    placement = 'top',
    inline_padding_left = 0,
    update_event = { 'DiagnosticChanged', 'BufReadPost' },
    toggle_event = { },
    show_sign = false,
    render_event = { 'DiagnosticChanged', 'CursorMoved' },
    border_chars = {
      top_left = "┌",
      top_right = "┐",
      bottom_left = "└",
      bottom_right = "┘",
      horizontal = "─",
      vertical = "│"
    },
    show_borders = false,
})
