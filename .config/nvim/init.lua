local vim = vim

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=main", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

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

local configure_global_keymaps = function ()
  local opts = { noremap = true, silent = true }
  local set = vim.keymap.set
  set("i", "<S-Tab>", "<C-\\><C-N><<<C-\\><C-N>^i", opts)
  set("n", "<Tab>",   ">>",  opts)
  set("n", "<S-Tab>", "<<",  opts)
  set("v", "<Tab>",   ">gv", opts)
  set("v", "<S-Tab>", "<gv", opts)
end

configure_global_keymaps()

-- Window management state
local window_state = {
  maximized_win = nil,  -- Currently maximized window ID
  saved_widths = {},    -- Saved widths before maximizing
  label_bufs = {},      -- Label buffers for collapsed windows
  dim_ns = vim.api.nvim_create_namespace('window_dim'),  -- Namespace for dimming
}

-- Create a dimmed highlight group
vim.api.nvim_set_hl(0, 'WindowDimmed', { fg = '#666666', bg = 'NONE' })
vim.api.nvim_set_hl(0, 'WindowDimmedBg', { bg = '#1a1a1a', fg = '#666666' })

local configure_window_management = function()
  local opts = { noremap = true, silent = true }
  local set = vim.keymap.set

  local collapsed_width = 3  -- Width for collapsed windows (visible with label)

  -- Get only vertical split windows (same row, different columns) in left-to-right order
  local function get_vertical_windows()
    local wins = vim.api.nvim_tabpage_list_wins(0)
    local vertical_wins = {}

    for _, win in ipairs(wins) do
      local config = vim.api.nvim_win_get_config(win)
      -- Skip floating windows
      if config.relative == '' then
        local pos = vim.api.nvim_win_get_position(win)
        table.insert(vertical_wins, { win = win, col = pos[2], row = pos[1] })
      end
    end

    -- Filter to only windows on the same row (vertical splits)
    if #vertical_wins > 0 then
      -- Group by row and take the largest group
      local row_groups = {}
      for _, w in ipairs(vertical_wins) do
        local row = w.row
        row_groups[row] = row_groups[row] or {}
        table.insert(row_groups[row], w)
      end

      -- Find the row with most windows (main vertical split row)
      local max_count = 0
      local main_row_wins = {}
      for _, group in pairs(row_groups) do
        if #group > max_count then
          max_count = #group
          main_row_wins = group
        end
      end

      -- Sort by column position (left to right)
      table.sort(main_row_wins, function(a, b) return a.col < b.col end)

      local result = {}
      for _, w in ipairs(main_row_wins) do
        table.insert(result, w.win)
      end
      return result
    end

    return {}
  end

  -- Create a label buffer showing the window number
  local function create_label_buffer(num)
    local buf = vim.api.nvim_create_buf(false, true)  -- nofile, scratch buffer
    vim.api.nvim_buf_set_option(buf, 'buftype', 'nofile')
    vim.api.nvim_buf_set_option(buf, 'bufhidden', 'wipe')
    vim.api.nvim_buf_set_option(buf, 'swapfile', false)
    vim.api.nvim_buf_set_name(buf, '[Window ' .. num .. ']')

    -- Create content with centered number
    local lines = {}
    for _ = 1, 50 do  -- Fill with empty lines
      table.insert(lines, '')
    end
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)

    return buf
  end

  -- Show window number labels in collapsed windows using winbar
  -- Also dim and disable syntax for collapsed windows
  local function update_window_labels()
    local wins = get_vertical_windows()

    for i, win in ipairs(wins) do
      if vim.api.nvim_win_is_valid(win) then
        local width = vim.api.nvim_win_get_width(win)
        local buf = vim.api.nvim_win_get_buf(win)
        -- Show label in collapsed windows
        if width <= collapsed_width + 2 then
          -- Use winbar to show the number prominently
          vim.api.nvim_win_set_option(win, 'winbar', '%=%#Title#[' .. tostring(i) .. ']%=')
          vim.api.nvim_win_set_option(win, 'number', false)
          vim.api.nvim_win_set_option(win, 'relativenumber', false)
          vim.api.nvim_win_set_option(win, 'signcolumn', 'no')
          vim.api.nvim_win_set_option(win, 'foldcolumn', '0')
          vim.api.nvim_win_call(win, function()
            vim.cmd('setlocal syntax=OFF')
          end)
        else
          vim.api.nvim_win_set_option(win, 'winbar', '')
          vim.api.nvim_win_set_option(win, 'number', true)
          vim.api.nvim_win_set_option(win, 'signcolumn', 'yes')
          vim.api.nvim_win_set_option(win, 'foldcolumn', '1')
          -- Restore normal highlighting
          vim.api.nvim_win_set_option(win, 'winhighlight', '')
          -- Re-enable syntax
          vim.api.nvim_win_call(win, function()
            vim.cmd('setlocal syntax=ON')
            -- Re-detect filetype to restore treesitter
            vim.cmd('filetype detect')
          end)
        end
      end
    end
  end

  -- Restore all windows to equal width
  local function equalize_windows()
    vim.cmd('wincmd =')
    window_state.maximized_win = nil
    window_state.saved_widths = {}
    update_window_labels()
  end

  -- Maximize a specific window, collapse others
  local function maximize_window(target_win)
    local wins = get_vertical_windows()
    if #wins <= 1 then return end

    local total_width = vim.o.columns
    local num_collapsed = #wins - 1
    local max_width = total_width - (num_collapsed * (collapsed_width + 1))  -- +1 for separator

    -- Save current widths if not already saved
    if vim.tbl_isempty(window_state.saved_widths) then
      for _, win in ipairs(wins) do
        if vim.api.nvim_win_is_valid(win) then
          window_state.saved_widths[win] = vim.api.nvim_win_get_width(win)
        end
      end
    end

    -- Set widths
    for _, win in ipairs(wins) do
      if vim.api.nvim_win_is_valid(win) then
        if win == target_win then
          vim.api.nvim_win_set_width(win, max_width)
        else
          vim.api.nvim_win_set_width(win, collapsed_width)
        end
      end
    end

    window_state.maximized_win = target_win
    update_window_labels()
  end

  -- Create new empty window on the right
  local function create_new_window()
    vim.cmd('botright vnew')  -- Create new vertical split with empty buffer on the right
    local new_win = vim.api.nvim_get_current_win()
    local new_buf = vim.api.nvim_get_current_buf()
    vim.api.nvim_buf_set_option(new_buf, 'buftype', '')  -- Normal buffer
    vim.api.nvim_buf_set_option(new_buf, 'buflisted', true)
    return new_win
  end

  -- Main function to handle window focus/create/maximize
  local function focus_window_by_number(n)
    local wins = get_vertical_windows()
    local current_win = vim.api.nvim_get_current_win()
    local num_wins = #wins

    -- If requesting a window beyond current count, create ONE new window
    if n > num_wins then
      create_new_window()
      wins = get_vertical_windows()
      num_wins = #wins
      -- Focus the new window (last one)
      local new_win = wins[#wins]
      if new_win and vim.api.nvim_win_is_valid(new_win) then
        vim.api.nvim_set_current_win(new_win)
      end
      return
    end

    -- If only one window and n=1, create a second one
    if num_wins <= 1 and n == 1 then
      create_new_window()
      wins = get_vertical_windows()
      return
    end

    -- Get target window
    local target_idx = math.min(n, #wins)
    local target_win = wins[target_idx]

    if not target_win or not vim.api.nvim_win_is_valid(target_win) then
      return
    end

    -- If target window is already focused
    if target_win == current_win then
      -- Toggle maximize/equalize behavior
      if window_state.maximized_win == target_win then
        -- Already maximized, equalize all
        equalize_windows()
      else
        -- Not maximized (or different window was maximized), maximize this one
        maximize_window(target_win)
      end
    else
      -- Focus the target window
      vim.api.nvim_set_current_win(target_win)

      -- If there's a maximized layout, maximize the newly focused window instead
      if window_state.maximized_win ~= nil then
        maximize_window(target_win)
      end
    end
  end

  -- Set up keybindings for window management
  -- Using F1-F9 function keys - works in all modes and terminals
  for i = 1, 9 do
    local fn = function() focus_window_by_number(i) end
    local desc = { desc = 'Focus/toggle window ' .. i }

    set({ 'n', 'i', 'v' }, '<F' .. i .. '>', fn, vim.tbl_extend('force', opts, desc))
  end

  -- Auto-update labels when windows change
  vim.api.nvim_create_autocmd({ 'WinEnter', 'WinResized', 'WinNew', 'WinClosed' }, {
    callback = function()
      vim.defer_fn(update_window_labels, 10)
    end,
  })
end

configure_window_management()

local configure_defaults = function (vim)
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

  vim.opt.keymodel="startsel,stopsel"

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

  vim.diagnostic.config({
    virtual_text = { current_line = true },
    update_in_insert = true,
    underline = true,
    severity_sort = true,
    virtual_lines = false
  })

  vim.filetype.add({
   extension = {
     props = 'msbuild',
     tasks = 'msbuild',
     targets = 'msbuild',
   },
   pattern = {
     [ [[.*\..*proj]] ] = 'msbuild',
   },
 })

end

configure_defaults(vim)

local tools = { 'clang-format', 'codelldb', 'copilot-language-server', 'yaml-language-server' }
local treesitter_configs = { 'c', 'cpp', 'rust', 'yaml', 'markdown', 'markdown_inline', 'typst', 'latex', 'html', 'typescript', 'javascript', 'regex', 'bash', 'lua', 'cmake', 'json', 'json5', 'jsonc' }
local lsp_configs = {
  clangd = {
    cmd = { 'clangd', '--background-index', '--clang-tidy', '--all-scopes-completion', '--pch-storage=memory', '--completion-style=detailed' },
    root_markers = { '.clangd', 'compile_commands.json', '.git', 'CMakeLists.txt' },
    filetypes = { 'c', 'cpp', 'h', 'hpp', 'cxx', 'c++' },
    single_file_support = true,
  },
  ['cmake-language-server'] = {
    cmd = { 'cmake-language-server' },
    filetypes = { 'cmake' },
    root_markers = { 'CMakePresets.json', 'CTestConfig.cmake', '.git', 'build', 'cmake' },
    init_options = {
      buildDirectory = 'build',
    },
  },
  copilot = {
    cmd = { 'copilot-language-server', '--stdio', },
    root_markers = { '.git' },
    single_file_support = true,
  },
  marksman = {
    cmd = { 'marksman' },
    root_markers = { '.marksman.toml', '.git', '*.md' },
    filetypes = { 'markdown', 'octo' },
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
  ['typescript-language-server'] = {
    cmd = { 'typescript-language-server', '--stdio' },
    root_markers = { 'package.json', 'jsconfig.json' },
    filetypes = { 'javascript', 'javascriptreact', 'javascript.jsx', 'typescript', 'typescriptreact', 'typescript.tsx' },
    single_file_support = true,
  },
  ['yaml-language-server'] = {
    cmd = { 'yaml-language-server', '--stdio' },
    filetypes = { 'yaml', 'yaml.docker-compose', 'yaml.gitlab', 'yaml.helm-values' },
    root_markers = { '.git' },
    settings = {
      redhat = { telemetry = { enabled = false } },
      yaml = { format = { enable = true } },
    },
    on_init = function(client)
      client.server_capabilities.documentFormattingProvider = true
    end,
  }
}

local plugins = {
  { 'nvim-lua/plenary.nvim' },
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    opts = {
      quickfile = {}
    }
  },
  {
    'wsdjeg/rooter.nvim',
    opts = {
      root_pattern = { '.git/' },
    }
  },
  {
    'projekt0n/github-nvim-theme',
    lazy = false,
    priority = 1000,
    config = function ()
      require('github-theme').setup({})
    end
  },
  {
    'f-person/auto-dark-mode.nvim',
    dependncies = { 'projekt0n/github-nvim-theme' },
    lazy = false,
    priority = 1000,
    opts = {
      update_interval = 1000,
      set_dark_mode = function ()
        vim.api.nvim_set_option_value("background", "dark", {})
        vim.cmd("colorscheme github_dark_high_contrast")
      end,
      set_light_mode = function ()
        vim.api.nvim_set_option_value("background", "light", {})
        vim.cmd("colorscheme github_light_default")
      end,
    }
  },
  {
    'y3owk1n/time-machine.nvim',
    cmd = {
      'TimeMachineToggle',
      'TimeMachinePurgeBuffer',
      'TimeMachinePurgeAll',
      'TimeMachineLogShow',
      'TimeMachineLogClear',
    },
    opts = {

    }
  },
  {
    'stevearc/oil.nvim',
    lazy = false,
    keys = {
      {
        'fe',
        mode = { 'n' },
        function() require('oil').open() end,
        desc = 'Open Oil file explorer',
      }
    },
    opts = {
      default_file_explorer = true,
      delete_to_trash = true,
      watch_for_changes = true,
      columns = {
        'permissions',
        'size',
        'mtime',
      },
      win_options = {
        signcolumn = 'auto:2'
      },

      view_options = {
        show_hidden = true
      }

    }
  },
  {
    'refractalize/oil-git-status.nvim',
    dependencies = {
      'stevearc/oil.nvim',
    },
    opts = {
      show_ignored = true
    }
  },
  {
    'dmtrKovalenko/fff.nvim',
    build = function()
      require("fff.download").download_or_build_binary()
    end,
    keys = {
      {
        "ff",
        mode = { 'n' },
        function() require('fff').find_files() end,
        desc = 'Find files',
      }
    },
    opts = {
      prompt = '> ',
      max_threads = vim.uv.available_parallelism()
    }
  },
  {
    'wsdjeg/flygrep.nvim',
    cmd = { 'FlyGrep' },
    dependencies = { 'wsdjeg/job.nvim' },
    opts = {
      enable_preview = true,
      input = vim.fn.expand('<cword>'),
      mappings = {
        next_item = '<Down>',
        prev_item = '<Up>',
      }
    },
    keys = {
      {
        "fg",
        function() require('flygrep').open() end,
        desc = 'Grep in files',
      }
    },
  }, { 'wsdjeg/job.nvim' },
  {
    'nvim-treesitter/nvim-treesitter',
    dependncies = { 'nvim-treesitter/nvim-treesitter-context' },
    build = ':TSUpdate',
    config = function ()
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
    end
  }, { 'nvim-treesitter/nvim-treesitter-context' },
  {
    'aaronik/treewalker.nvim',
    opts = { },
    keys = {
      { '<C-k>', '<cmd>Treewalker Up<cr>',        mode = { 'n', 'v', 'i' }, silent = true },
      { '<C-j>', '<cmd>Treewalker Down<cr>',      mode = { 'n', 'v', 'i' }, silent = true },
      -- { '<C-h>', '<cmd>Treewalker Left<cr>',      mode = { 'n', 'v', 'i' }, silent = true },
      -- { '<C-l>', '<cmd>Treewalker Right<cr>',     mode = { 'n', 'v', 'i' }, silent = true },
      { '<C-Up>', '<cmd>Treewalker Up<cr>',       mode = { 'n', 'v', 'i' }, silent = true },
      { '<C-Down>', '<cmd>Treewalker Down<cr>',   mode = { 'n', 'v', 'i' }, silent = true },
      -- { '<C-Left>', '<cmd>Treewalker Left<cr>',   mode = { 'n', 'v', 'i' }, silent = true },
      -- { '<C-Right>', '<cmd>Treewalker Right<cr>', mode = { 'n', 'v', 'i' }, silent = true },

      { '<C-S-k>', '<cmd>Treewalker SwapUp<cr>',        mode = { 'n', 'v', 'i' }, silent = true },
      { '<C-S-j>', '<cmd>Treewalker SwapDown<cr>',      mode = { 'n', 'v', 'i' }, silent = true },
      { '<C-S-h>', '<cmd>Treewalker SwapLeft<cr>',      mode = { 'n', 'v', 'i' }, silent = true },
      { '<C-S-l>', '<cmd>Treewalker SwapRight<cr>',     mode = { 'n', 'v', 'i' }, silent = true },
      { '<C-S-Up>', '<cmd>Treewalker SwapUp<cr>',       mode = { 'n', 'v', 'i' }, silent = true },
      { '<C-S-Down>', '<cmd>Treewalker SwapDown<cr>',   mode = { 'n', 'v', 'i' }, silent = true },
      { '<C-S-Left>', '<cmd>Treewalker SwapLeft<cr>',   mode = { 'n', 'v', 'i' }, silent = true },
      { '<C-S-Right>', '<cmd>Treewalker SwapRight<cr>', mode = { 'n', 'v', 'i' }, silent = true },
    }
  },
  {
    'mason-org/mason.nvim',
    opts = {}
  },
  {
    'WhoIsSethDaniel/mason-tool-installer.nvim',
    build = ':MasonUpdate',
    dependncies = { 'mason-org/mason.nvim' },
    config = function ()
      require('mason').setup()
      local all_tools = merge_table(vim.tbl_keys(lsp_configs), tools)
      for _, tool in pairs(all_tools) do
          if vim.fn.executable(tool) == 0 then
              print("Installing tool: " .. tool)
              vim.cmd("MasonInstall " .. tool)
          end
      end

    end
  },
  {
    'https://github.com/j-hui/fidget.nvim',
    opts = {
      suppress_on_insert = true,
      ignore_done_already = true,
      ignore_empty_message = true,
    }
  },

  {
    'MeanderingProgrammer/render-markdown.nvim',
    ft = { 'markdown', 'quarto', 'octo' },
    opts = {
      render_modes = { 'n', 'c', 't' },
      anti_conceal = { enabled = false },
      completions = { lsp = { enabled = true } },
    }
  },
  {
    "zbirenbaum/copilot.lua",
    dependncies = { 'copilotlsp-nvim/copilot-lsp' },
    cmd = "Copilot",
    event = "InsertEnter",
    opts = {
      suggestion = {
        enabled = true,
        auto_trigger = true,
      },
      nes = {
        enabled = false,
        keymap = {
          accept_and_goto = "",
          accept = false,
          dismiss = "<Esc>",
        },
      },
    }
  }, { 'copilotlsp-nvim/copilot-lsp' },
  {
    "chrisgrieser/nvim-origami",
    event = "VeryLazy",
    init = function()
      vim.opt.foldlevel = 99
      vim.opt.foldlevelstart = 99
    end,
    keys = {
      { "<Left>",  function() require("origami").h() end, mode = { 'n' } },
      { "<Right>", function() require("origami").l() end, mode = { 'n' } }

    },
    opts = {
      useLspFoldsWithTreesitterFallback = true,
      pauseFoldsOnSearch = true,
      foldtext = {
        enabled = true,
        padding = 3,
        lineCount = {
            template = "%d lines",
            hlgroup = "Comment",
        },
        diagnosticsCount = true,
        gitsignsCount = true,
      },
      autoFold = {
        enabled = true,
        kinds = { "comments", "imports" }
      }
    },
  },
  {
    'windwp/nvim-autopairs',
    event = "InsertEnter",
    config = true,
    opts = {}
  },
  {
    'kylechui/nvim-surround',
    event = 'VeryLazy',
    opts = {}
  },
  {
    'qwavies/smart-backspace.nvim',
    event = {'InsertEnter', 'CmdlineEnter'},
    opts = {}
  },
  {
    'lewis6991/gitsigns.nvim',
    lazy = false,
    opts = {
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
  },
  {
    'sindrets/diffview.nvim',
    event = 'VeryLazy',
    opts = {
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
    }
  },
  {
    'NeogitOrg/neogit',
    cmd = 'Neogit',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'sindrets/diffview.nvim',
    },
    opts = {},
    config = function ()
      vim.cmd.cabbrev('git', 'Neogit')
      vim.cmd.cabbrev('Git', 'Neogit')

      vim.api.nvim_create_user_command(
        'Git',
        'Neogit',
        { bang = true, desc = "Alias to Neogit" }
      )
    end
  },
  {
    'pwntester/octo.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'folke/snacks.nvim'
    },
    cmd = 'Octo',
    opts = {
      picker = 'snacks',
      enable_builtin = true,
    }
  }
}

require('lazy').setup({ install = { missing = true, colorscheme = { 'github-theme' } }, spec = plugins, checker = { enabled = true } })

local configure_lsp = function(vim, lsp_configs)
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
      local group = vim.api.nvim_create_augroup("UserLspAttach", { clear = true })

      local client = vim.lsp.get_client_by_id(args.data.client_id)
      local bufnr = args.buf

      if client:supports_method(vim.lsp.protocol.Methods.textDocument_completion, bufnr) then
        vim.lsp.completion.enable(true, client.id, bufnr, { autotrigger = true })
        vim.keymap.set('i', '<Tab>',
          function()
            if not vim.lsp.inline_completion.get() then
              return '<Tab>'
            end
          end, { expr = true, desc = 'Accept the current inline completion' })
      end

      if client:supports_method(vim.lsp.protocol.Methods.textDocument_inlineCompletion, bufnr) then
        vim.lsp.inline_completion.enable(true, { bufnr = bufnr })
      end

      if client:supports_method(vim.lsp.protocol.Methods.textDocument_foldingRange, bufnr) then
        local win = vim.api.nvim_get_current_win()
        vim.wo[win][0].foldmethod = 'expr'
        vim.wo[win][0].foldexpr = 'v:lua.vim.lsp.foldexpr()'
      end

      if client:supports_method(vim.lsp.protocol.Methods.textDocument_documentColor, bufnr) then
        vim.lsp.document_color.enable(true, bufnr, { style = 'virtual' })
      end

      if client:supports_method(vim.lsp.protocol.Methods.textDocument_semanticTokens_full, bufnr) then
        vim.lsp.semantic_tokens.enable(true, { bufnr = bufnr })
      end

      if client:supports_method(vim.lsp.protocol.Methods.textDocument_onTypeFormatting, bufnr) and vim.lsp.on_type_formatting then
        vim.lsp.on_type_formatting.enable(true, { client_id = client.id })
      end

      if client:supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight, bufnr) then
        vim.api.nvim_create_autocmd('CursorHold',  { callback = function () vim.lsp.buf.document_highlight() end, })
        vim.api.nvim_create_autocmd('CursorHoldI', { callback = function () vim.lsp.buf.document_highlight() end, })
        vim.api.nvim_create_autocmd('CursorMoved', {
          buffer = bufnr,
          callback = function() vim.lsp.buf.clear_references() end,
        })
      end
    end,
  })
  vim.api.nvim_create_autocmd('LspDetach',   { command = 'setl foldexpr<' })
  vim.api.nvim_create_autocmd("VimLeavePre", { callback = function () vim.iter(vim.lsp.get_clients()):each(function(client) client:stop() end) end, })
  vim.api.nvim_create_autocmd('CursorMoved', { callback = function () vim.lsp.buf.clear_references() end, })

  vim.lsp.enable(vim.tbl_keys(lsp_configs))

  if vim.g.lsp_on_demands then
    vim.lsp.enable(vim.g.lsp_on_demands)
  end
end

configure_lsp(vim, lsp_configs)

--[=====[

local plugins = {

  { 'https://github.com/echasnovski/mini.nvim' },
  {
    'https://github.com/echasnovski/mini.pick',
    config = function ()
      require('mini.pick').setup({})
    end,
  },
  { 'https://github.com/nvim-lua/plenary.nvim' },
  { 'https://github.com/nvim-telescope/telescope.nvim' },
  { 'https://github.com/folke/snacks.nvim' },


  { src = 'https://github.com/MunifTanjim/nui.nvim' },
  { src = 'https://github.com/rcarriga/nvim-notify' },
  { src = 'https://github.com/folke/noice.nvim' },

  { src = 'https://github.com/stevearc/oil.nvim' },

  { src = 'https://github.com/tkancf/narrowing-nvim' },

  { src = 'https://github.com/amitds1997/remote-nvim.nvim' },

  { src = 'https://github.com/nvim-tree/nvim-web-devicons' },

  { src = 'https://github.com/mason-org/mason.nvim' },
  { src = 'https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim' },

  { src = 'https://github.com/nvim-treesitter/nvim-treesitter' },
  { src = 'https://github.com/nvim-treesitter/nvim-treesitter-context' },

  { src = 'https://github.com/MeanderingProgrammer/render-markdown.nvim' },

  { src = 'https://github.com/onsails/lspkind.nvim' },
  { src = 'https://github.com/jinzhongjia/LspUI.nvim', version = 'main' },

  { src = 'https://github.com/yetone/avante.nvim' },

  { src = 'https://github.com/Bekaboo/dropbar.nvim' },

  { src = 'https://github.com/y3owk1n/time-machine.nvim' },

  { src = 'https://github.com/sindrets/diffview.nvim' },
  { src = 'https://github.com/lewis6991/gitsigns.nvim' },
  { src = 'https://github.com/pwntester/octo.nvim' },
  { src = 'https://github.com/otavioschwanck/github-pr-reviewer.nvim' },
  { src = 'https://github.com/NeogitOrg/neogit' },

  { src = 'https://github.com/shortcuts/no-neck-pain.nvim' },

  { src = 'https://github.com/code-biscuits/nvim-biscuits' },

  { src = 'https://github.com/dgagn/diagflow.nvim' },

  { src = 'https://github.com/MagicDuck/grug-far.nvim' },

  { src = 'https://github.com/A7Lavinraj/fyler.nvim' },
  { src = 'https://github.com/stevearc/oil.nvim' }
}

vim.pack.add(plugins)

vim.cmd.cabbrev('git', 'Neogit')
vim.cmd.cabbrev('Git', 'Neogit')

vim.api.nvim_create_user_command(
  'Git',
  'Neogit',
  { bang = true, desc = "Alias to Neogit" }
)




require('nvim-web-devicons').setup({
  default = true
})


require("oil").setup()

require('narrowing').setup({keymaps = { enabled = true }})

require('remote-nvim').setup({})

vim.keymap.set('i', '<c-space>', vim.lsp.completion.get)




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
require('github-pr-reviewer').setup({})
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
  messages = {
    view_search = false
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

require('grug-far').setup({})

require("fyler").setup({})
require("oil").setup({})
--]=====]