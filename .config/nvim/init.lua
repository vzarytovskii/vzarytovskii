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
  --set("n", "<Tab>",   ">>",  opts)
  --set("n", "<S-Tab>", "<<",  opts)
  --set("v", "<Tab>",   ">gv", opts)
  --set("v", "<S-Tab>", "<gv", opts)
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
          vim.api.nvim_set_option_value('winbar', '%=%#Title#[' .. tostring(i) .. ']%=', { win = win, scope = 'local' })
          vim.api.nvim_set_option_value('number', false, { win = win, scope = 'local' })
          vim.api.nvim_set_option_value('relativenumber', false, { win = win, scope = 'local' })
          vim.api.nvim_set_option_value('signcolumn', 'no', { win = win, scope = 'local' })
          vim.api.nvim_set_option_value('foldcolumn', '0', { win = win, scope = 'local' })
          vim.api.nvim_win_call(win, function()
            vim.cmd('setlocal syntax=OFF')
          end)
        else
          vim.api.nvim_set_option_value('winbar', '', { win = win, scope = 'local' })
          vim.api.nvim_set_option_value('number', true, { win = win, scope = 'local' })
          vim.api.nvim_set_option_value('signcolumn', 'yes', { win = win, scope = 'local' })
          vim.api.nvim_set_option_value('foldcolumn', '1', { win = win, scope = 'local' })
          vim.api.nvim_set_option_value('winhighlight', '', { win = win, scope = 'local' })
          vim.api.nvim_win_call(win, function()
            vim.cmd('setlocal syntax=ON')
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
  local is_mac = vim.fn.has('macunix') == 1

  for i = 1, 9 do
    local fn = function() focus_window_by_number(i) end
    local desc = { desc = 'Focus/toggle window ' .. i }

    if vim.g.neovide then
      -- Neovide: Use Cmd+number (D = Cmd on macOS in Neovide)
      set({ 'n', 'i', 'v' }, '<D-' .. i .. '>', fn, vim.tbl_extend('force', opts, desc))
    end

    -- Windows/Linux: Use Alt+number (works in most terminals)
    if not is_mac then
      set({ 'n', 'i', 'v' }, '<M-' .. i .. '>', fn, vim.tbl_extend('force', opts, desc))
    end

    -- Always keep F1-F9 as fallback (works everywhere)
    set({ 'n', 'i', 'v' }, '<F' .. i .. '>', fn, vim.tbl_extend('force', opts, desc))
  end

  -- macOS terminal: Option+number sends Unicode characters, map them
  -- Option+1=¡, Option+2=™, Option+3=£, Option+4=¢, Option+5=∞, Option+6=§, Option+7=¶, Option+8=•, Option+9=ª
  if is_mac and not vim.g.neovide then
    local mac_opt_chars = { '¡', '™', '£', '¢', '∞', '§', '¶', '•', 'ª' }
    for i, char in ipairs(mac_opt_chars) do
      local fn = function() focus_window_by_number(i) end
      set({ 'n', 'i', 'v' }, char, fn, vim.tbl_extend('force', opts, { desc = 'Focus/toggle window ' .. i }))
    end
  end
end

configure_window_management()

local configure_defaults = function (vim)
  vim.g.mapleader = " "

  vim.opt.complete = 'o'
  vim.opt.pumheight = 7
  vim.opt.pummaxwidth = 80
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
    vim.g.neovide_padding_top = 1
    vim.g.neovide_padding_bottom = 1
    vim.g.neovide_padding_right = 1
    vim.g.neovide_padding_left = 1
    vim.g.neovide_opacity = 0.8
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


  --vim.api.nvim_create_autocmd("CmdlineChanged", {
  --  pattern = "/",
  --  callback = function()
  --    vim.fn.wildtrigger()
  --  end,
  --})
  vim.opt.smartcase = true
  vim.opt.ignorecase = true

  vim.opt.wildoptions = "pum,fuzzy,exacttext"
  vim.opt.wildmode = "longest:full,full"


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
      bigfile = { enabled = false },
      explorer = { enabled = false },
      input = { enabled = true },
      image = { enabled = false },
      lazygit = { enabled = false },
      quickfile = { enabled = true },
      scroll = { enabled = false },
      statuscolumn = { enabled = false },
      notifier = { enabled = true },
      picker = { enabled = true },
    },
    keys = {
      { '<leader><space>', mode = { 'n', 'v' }, function() Snacks.picker.smart() end, desc = 'Smart Find Files' },
      { '<leader>ff', mode = { 'n', 'v' }, function() Snacks.picker.smart() end, desc = 'Smart Find Files' },
      { 'ff', mode = { 'n', 'v' }, function() Snacks.picker.smart() end, desc = 'Smart Find Files' },

      { "<leader>/", mode = { 'n', 'v' }, function() Snacks.picker.grep() end, desc = "Grep" },
      { "<leader>gg", mode = { 'n', 'v' }, function() Snacks.picker.grep() end, desc = "Grep" },
      { "gg", mode = { 'n', 'v' }, function() Snacks.picker.grep() end, desc = "Grep" },
      { "ß", mode = { 'n', 'v', 'i' }, function() Snacks.picker.grep() end, desc = "Grep" },

      { "<leader>e", mode = { 'n', 'v' }, function() Snacks.explorer() end, desc = "File Explorer" },

      { 'gd', mode = { 'n', 'v' }, function() Snacks.picker.lsp_definitions() end, desc = 'Goto Definition' },
      { 'gD', mode = { 'n', 'v' }, function() Snacks.picker.lsp_declarations() end, desc = 'Goto Declaration' },
      { 'gr', mode = { 'n', 'v' }, function() Snacks.picker.lsp_references() end, nowait = true, desc = 'References' },
      { 'gI', mode = { 'n', 'v' }, function() Snacks.picker.lsp_implementations() end, desc = 'Goto Implementation' },
      { 'gy', mode = { 'n', 'v' }, function() Snacks.picker.lsp_type_definitions() end, desc = 'Goto T[y]pe Definition' },
      { 'gai', mode = { 'n', 'v' }, function() Snacks.picker.lsp_incoming_calls() end, desc = 'C[a]lls Incoming' },
      { 'gao', mode = { 'n', 'v' }, function() Snacks.picker.lsp_outgoing_calls() end, desc = 'C[a]lls Outgoing' },
      { '<leader>ss', mode = { 'n', 'v' }, function() Snacks.picker.lsp_symbols() end, desc = 'LSP Symbols' },
      { '<leader>sS', mode = { 'n', 'v' }, function() Snacks.picker.lsp_workspace_symbols() end, desc = 'LSP Workspace Symbols' },

      { "<leader>sd", mode = { 'n', 'v' }, function() Snacks.picker.diagnostics() end, desc = "Diagnostics" },

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
    'folke/flash.nvim',
    event = 'VeryLazy',
    ---@type Flash.Config
    opts = {},
    keys = {
      { 's', mode = { 'n', 'x', 'o' }, function() require('flash').jump() end, desc = 'Flash' },
      { 'S', mode = { 'n', 'x', 'o' }, function() require('flash').treesitter() end, desc = 'Flash Treesitter' },
    },
  },
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
    'andersevenrud/nvim_context_vt',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    opts = {
      enable = true,
      prefix = '// '
    }
  },
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
		event = "BufRead",
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
    cmd = { "DiffviewOpen", "DiffviewFileHistory" },
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
      'folke/snacks.nvim'
    },
    opts = {
      graph_style = 'unicode',
      process_spinner = true,
      highlight = {
        italic = false,
        bold = true,
        underline = true
      },
      integrations = {
        diffview = true,
        mini_pick = true,
        snacks = true
      }
    },
    config = function ()
      vim.cmd.cabbrev('git', 'Neogit')
      vim.cmd.cabbrev('Git', 'Neogit')

      vim.api.nvim_create_user_command(
        'Git',
        'Neogit',
        { bang = true, desc = 'Alias to Neogit' }
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
