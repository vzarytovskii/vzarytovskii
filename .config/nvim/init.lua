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

local window_state = {
  maximized_win = nil,
  saved_widths = {},
  label_bufs = {},
  collapsed_wins = {},
}

vim.api.nvim_set_hl(0, 'WindowDimmed', { fg = '#666666', bg = 'NONE' })
vim.api.nvim_set_hl(0, 'WindowDimmedBg', { bg = '#1a1a1a', fg = '#666666' })

local configure_window_management = function()
  local opts = { noremap = true, silent = true }
  local set = vim.keymap.set

  local collapsed_width = 3

  local function get_vertical_windows()
    local wins = vim.api.nvim_tabpage_list_wins(0)
    local vertical_wins = {}

    for _, win in ipairs(wins) do
      local config = vim.api.nvim_win_get_config(win)
      if config.relative == '' then
        local pos = vim.api.nvim_win_get_position(win)
        table.insert(vertical_wins, { win = win, col = pos[2], row = pos[1] })
      end
    end

    if #vertical_wins > 0 then
      local row_groups = {}
      for _, w in ipairs(vertical_wins) do
        local row = w.row
        row_groups[row] = row_groups[row] or {}
        table.insert(row_groups[row], w)
      end

      local max_count = 0
      local main_row_wins = {}
      for _, group in pairs(row_groups) do
        if #group > max_count then
          max_count = #group
          main_row_wins = group
        end
      end

      table.sort(main_row_wins, function(a, b) return a.col < b.col end)

      local result = {}
      for _, w in ipairs(main_row_wins) do
        table.insert(result, w.win)
      end
      return result
    end

    return {}
  end

  local function create_label_buffer(num)
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_option(buf, 'buftype', 'nofile')
    vim.api.nvim_buf_set_option(buf, 'bufhidden', 'wipe')
    vim.api.nvim_buf_set_option(buf, 'swapfile', false)
    vim.api.nvim_buf_set_name(buf, '[Window ' .. num .. ']')

    local lines = {}
    for _ = 1, 50 do
      table.insert(lines, '')
    end
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)

    return buf
  end

  local function restore_window_to_global(win)
    if not vim.api.nvim_win_is_valid(win) then return end
    vim.api.nvim_win_call(win, function()
      vim.cmd('setlocal winbar<')
      vim.cmd('setlocal number<')
      vim.cmd('setlocal relativenumber<')
      vim.cmd('setlocal signcolumn<')
      vim.cmd('setlocal foldcolumn<')
      vim.cmd('setlocal winhighlight<')
      vim.cmd('setlocal syntax=ON')
    end)
    window_state.collapsed_wins[win] = nil
  end

  local function restore_all_collapsed_windows()
    for win, _ in pairs(window_state.collapsed_wins) do
      restore_window_to_global(win)
    end
    window_state.collapsed_wins = {}
    window_state.maximized_win = nil
    window_state.saved_widths = {}
  end

  local function update_window_labels()
    local wins = get_vertical_windows()

    for i, win in ipairs(wins) do
      if vim.api.nvim_win_is_valid(win) then
        local width = vim.api.nvim_win_get_width(win)
        local buf = vim.api.nvim_win_get_buf(win)
        if width <= collapsed_width + 2 then
          vim.api.nvim_set_option_value('winbar', '%=%#Title#[' .. tostring(i) .. ']%=', { win = win, scope = 'local' })
          vim.api.nvim_set_option_value('number', false, { win = win, scope = 'local' })
          vim.api.nvim_set_option_value('relativenumber', false, { win = win, scope = 'local' })
          vim.api.nvim_set_option_value('signcolumn', 'no', { win = win, scope = 'local' })
          vim.api.nvim_set_option_value('foldcolumn', '0', { win = win, scope = 'local' })
          vim.api.nvim_win_call(win, function()
            vim.cmd('setlocal syntax=OFF')
          end)
          window_state.collapsed_wins[win] = true
        else
          restore_window_to_global(win)
        end
      end
    end
  end

  local function equalize_windows()
    vim.cmd('wincmd =')
    window_state.maximized_win = nil
    window_state.saved_widths = {}
    update_window_labels()
  end

  local function maximize_window(target_win)
    local wins = get_vertical_windows()
    if #wins <= 1 then return end

    local total_width = vim.o.columns
    local num_collapsed = #wins - 1
    local max_width = total_width - (num_collapsed * (collapsed_width + 1))

    if vim.tbl_isempty(window_state.saved_widths) then
      for _, win in ipairs(wins) do
        if vim.api.nvim_win_is_valid(win) then
          window_state.saved_widths[win] = vim.api.nvim_win_get_width(win)
        end
      end
    end

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

  local function create_new_window()
    vim.cmd('botright vnew')
    local new_win = vim.api.nvim_get_current_win()
    local new_buf = vim.api.nvim_get_current_buf()
    vim.api.nvim_buf_set_option(new_buf, 'buftype', '')
    vim.api.nvim_buf_set_option(new_buf, 'buflisted', true)
    return new_win
  end

  local function focus_window_by_number(n)
    local wins = get_vertical_windows()
    local current_win = vim.api.nvim_get_current_win()
    local num_wins = #wins

    if n > num_wins then
      create_new_window()
      wins = get_vertical_windows()
      num_wins = #wins
      local new_win = wins[#wins]
      if new_win and vim.api.nvim_win_is_valid(new_win) then
        vim.api.nvim_set_current_win(new_win)
      end
      return
    end

    if num_wins <= 1 and n == 1 then
      create_new_window()
      wins = get_vertical_windows()
      return
    end

    local target_idx = math.min(n, #wins)
    local target_win = wins[target_idx]

    if not target_win or not vim.api.nvim_win_is_valid(target_win) then
      return
    end

    if target_win == current_win then
      if window_state.maximized_win == target_win then
        equalize_windows()
      else
        maximize_window(target_win)
      end
    else
      vim.api.nvim_set_current_win(target_win)

      if window_state.maximized_win ~= nil then
        maximize_window(target_win)
      end
    end
  end

  local is_mac = vim.fn.has('macunix') == 1

  for i = 1, 9 do
    local fn = function() focus_window_by_number(i) end
    local desc = { desc = 'Focus/toggle window ' .. i }

    if vim.g.neovide then
      set({ 'n', 'i', 'v' }, '<D-' .. i .. '>', fn, vim.tbl_extend('force', opts, desc))
    end

    if not is_mac then
      set({ 'n', 'i', 'v' }, '<M-' .. i .. '>', fn, vim.tbl_extend('force', opts, desc))
    end

    set({ 'n', 'i', 'v' }, '<F' .. i .. '>', fn, vim.tbl_extend('force', opts, desc))
  end

  if is_mac and not vim.g.neovide then
    local mac_opt_chars = { '¡', '™', '£', '¢', '∞', '§', '¶', '•', 'ª' }
    for i, char in ipairs(mac_opt_chars) do
      local fn = function() focus_window_by_number(i) end
      set({ 'n', 'i', 'v' }, char, fn, vim.tbl_extend('force', opts, { desc = 'Focus/toggle window ' .. i }))
    end
  end

  vim.api.nvim_create_autocmd('WinClosed', {
    callback = function(args)
      local closed_win = tonumber(args.match)
      if closed_win == window_state.maximized_win then
        vim.defer_fn(restore_all_collapsed_windows, 10)
      elseif window_state.collapsed_wins[closed_win] then
        window_state.collapsed_wins[closed_win] = nil
        vim.defer_fn(function()
          local wins = get_vertical_windows()
          if #wins <= 1 then
            restore_all_collapsed_windows()
          end
        end, 10)
      end
    end,
  })
end

configure_window_management()

local configure_defaults = function (vim)
  vim.g.mapleader = " "

  vim.opt.updatetime = 500

  vim.opt.encoding = "utf-8"
  vim.opt.fileencodings = 'utf-8,utf-16,utf-32,ucs-bom,default,latin'

  vim.opt.fileformat = 'unix'
  vim.opt.fileformats = 'unix,dos,mac'

  vim.opt.pumblend = 0
  vim.opt.pumborder = 'rounded'
  vim.opt.pumheight = 7
  vim.opt.pummaxwidth = 180
  vim.opt.pumwidth = 80

  vim.opt.autocomplete = true
  vim.opt.complete = 'o,F,.,i,d'
  vim.opt.completeopt = { 'fuzzy', 'menu', 'menuone', 'noselect', 'preinsert', 'popup' }
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
  vim.opt.signcolumn = "yes:1"
  vim.opt.isfname:append("@-@")

  vim.opt.keymodel="startsel,stopsel"

  vim.opt.undofile = true
  vim.opt.undodir = vim.fn.expand("~/.undodir")

  vim.opt.number = true

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

local treesitter_configs = { 'c', 'cpp', 'git_config', 'git_rebase', 'gitattributes', 'gitcommit', 'gitignore', 'rust', 'yaml', 'markdown', 'markdown_inline', 'regex', 'bash', 'lua', 'cmake', 'json', 'json5', 'jsonc', 'powershell', 'xml' }
local tools = { 'clang-format', 'codelldb' }
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
  ['copilot-language-server'] = {
    cmd = { 'copilot-language-server', '--stdio', },
    root_markers = { '.git' },
    single_file_support = true,
  },
  ['lua-language-server'] = {
    cmd = { 'lua-language-server' },
    filetypes = { 'lua' },
    root_markers = { '.git', 'lua' },
    settings = {
      Lua = {
        runtime = {
          version = 'LuaJIT',
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
  },
  marksman = {
    cmd = { 'marksman' },
    root_markers = { '.marksman.toml', '.git', '*.md' },
    filetypes = { 'markdown' },
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
  { 'MunifTanjim/nui.nvim' },
  {
    'nvim-telescope/telescope.nvim',
    dependencies = { 'nvim-lua/plenary.nvim', 'nvim-telescope/telescope-fzf-native.nvim' },
    priority = 1000,
    lazy = false,
    opts = {
      extensions = {
        fzf = {
          fuzzy = true,
          override_generic_sorter = true,
          override_file_sorter = true,
          case_mode = "smart_case",
        }
      }
    },
    keys = {
      { '<leader><space>', mode = { 'n', 'v' }, function() require('telescope.builtin').find_files() end, desc = 'Find Files' },
      { '<leader>ff', mode = { 'n', 'v' }, function() require('telescope.builtin').find_files() end, desc = 'Find Files' },
      { 'ff', mode = { 'n', 'v' }, function() require('telescope.builtin').find_files() end, desc = 'Find Files' },

      { "<leader>/", mode = { 'n', 'v' }, function() require('telescope.builtin').live_grep() end, desc = "Grep" },
      { "<leader>gg", mode = { 'n', 'v' }, function() require('telescope.builtin').live_grep() end, desc = "Grep" },
      { "gg", mode = { 'n', 'v' }, function() require('telescope.builtin').live_grep() end, desc = "Grep" },
      { "ß", mode = { 'n', 'v', 'i' }, function() require('telescope.builtin').live_grep() end, desc = "Grep" },

      { 'gd', mode = { 'n', 'v' }, function() require('telescope.builtin').lsp_definitions() end, desc = 'Goto Definition' },
      { 'gr', mode = { 'n', 'v' }, function() require('telescope.builtin').lsp_references() end, nowait = true, desc = 'References' },
      { 'gI', mode = { 'n', 'v' }, function() require('telescope.builtin').lsp_implementations() end, desc = 'Goto Implementation' },
      { 'gy', mode = { 'n', 'v' }, function() require('telescope.builtin').lsp_type_definitions() end, desc = 'Goto T[y]pe Definition' },
      { 'gai', mode = { 'n', 'v' }, function() require('telescope.builtin').lsp_incoming_calls() end, desc = 'C[a]lls Incoming' },
      { 'gao', mode = { 'n', 'v' }, function() require('telescope.builtin').lsp_outgoing_calls() end, desc = 'C[a]lls Outgoing' },
      { '<leader>ss', mode = { 'n', 'v' }, function() require('telescope.builtin').lsp_document_symbols() end, desc = 'LSP Symbols' },
      { '<leader>sS', mode = { 'n', 'v' }, function() require('telescope.builtin').lsp_workspace_symbols() end, desc = 'LSP Workspace Symbols' },

      { "<leader>sd", mode = { 'n', 'v' }, function() require('telescope.builtin').diagnostics() end, desc = "Diagnostics" },

    }
  }, { 'nvim-telescope/telescope-fzf-native.nvim', build = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release --target install' },
  {
    'wsdjeg/rooter.nvim',
    lazy = false,
    opts = {
      root_pattern = { '.git/', 'Cargo.toml', 'README.md' },
    }
  },
  {
    'Mofiqul/vscode.nvim',
    lazy = false,
    priority = 1000,
    config = function ()
      vim.cmd("colorscheme vscode")
    end,
    opts = {
      transparent = true,
      italic_comments = false,
      intalic_inlayhints = false,
      terminal_colors = true
    }
  },
  {
    'f-person/auto-dark-mode.nvim',
    dependncies = { 'Mofiqul/vscode.nvim' },
    lazy = false,
    priority = 1000,
    opts = {
      update_interval = 1000,
      set_dark_mode = function ()
        vim.api.nvim_set_option_value("background", "dark", {})
        vim.cmd("colorscheme vscode")
      end,
      set_light_mode = function ()
        vim.api.nvim_set_option_value("background", "light", {})
        vim.cmd("colorscheme vscode")
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
    event = { 'VeryLazy' },
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
    branch = 'main',
    dependncies = { 'nvim-treesitter/nvim-treesitter-context' },
    lazy = false,
    build = ':TSUpdate',
    config = function ()
      vim.treesitter.language.register('markdown', 'octo')
      vim.treesitter.language.register('xml', 'msbuild')

      local ts = require('nvim-treesitter')
      ts.install(treesitter_configs)

      local group = vim.api.nvim_create_augroup('TreesitterSetup', { clear = true })

      local ignore_filetypes = {
        'checkhealth',
        'lazy',
        'mason',
        'snacks_dashboard',
        'snacks_notif',
        'snacks_win',
      }

      vim.api.nvim_create_autocmd('FileType', {
        group = group,
        desc = 'Enable treesitter highlighting and indentation',
        callback = function(event)
          if vim.tbl_contains(ignore_filetypes, event.match) then
            return
          end
          local lang = vim.treesitter.language.get_lang(event.match) or event.match
          local buf = event.buf
          pcall(vim.treesitter.start, buf, lang)
          vim.bo[buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
          ts.install({ lang })
        end,
      })

    end
  }, { 'nvim-treesitter/nvim-treesitter-context' },
  {
    'andersevenrud/nvim_context_vt',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    event = { 'BufReadPost', 'BufNewFile' },
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
      { '<C-Up>', '<cmd>Treewalker Up<cr>',       mode = { 'n', 'v', 'i' }, silent = true },
      { '<C-Down>', '<cmd>Treewalker Down<cr>',   mode = { 'n', 'v', 'i' }, silent = true },

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
    cmd = { 'Mason', 'MasonInstall', 'MasonUninstall', 'MasonUpdate', 'MasonLog' },
    opts = {}
  },
  {
    'WhoIsSethDaniel/mason-tool-installer.nvim',
    dependncies = { 'mason-org/mason.nvim' },
    cmd = { 'MasonToolsClean', 'MasonToolsInstall', 'MasonToolsUpdate', 'MasonToolsInstallSync', 'MasonToolsUpdateSync' },
    build = ':MasonToolsInstall',
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
    'j-hui/fidget.nvim',
    event = { 'LspAttach' },
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
    'zbirenbaum/copilot.lua',
    dependencies = { 'copilotlsp-nvim/copilot-lsp' },
    cmd = 'Copilot',
    event = { 'InsertEnter', 'LspAttach' },
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
      useLspFoldsWithTreesitterFallback = {
        enabled = true
      },
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
      linehl     = false,
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
    'esmuellert/vscode-diff.nvim',
    enabled = false,
    branch = 'next',
    dependencies = { 'MunifTanjim/nui.nvim' },
    cmd = 'CodeDiff',
    opts = {}
  },
  {
    'sindrets/diffview.nvim',
    cmd = { 'DiffviewOpen', 'DiffviewFileHistory' },
    opts = function ()
      local actions = require('diffview.actions')
      return {
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
        },
        hooks = {
          -- do not fold
          diff_buf_win_enter = function(bufnr)
            vim.opt_local.foldenable = false
          end
        },
        keymaps = {
          file_history_panel = {
            {
              "n",
              "<cr>",
              actions.focus_entry,
              { desc = "Open and focus the diff for the selected entry." },
            }
          },
        }
      }
  end
  },
  {
    'NeogitOrg/neogit',
    cmd = 'Neogit',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'sindrets/diffview.nvim',
      'nvim-telescope/telescope.nvim'
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
        telescope = true
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
      'nvim-telescope/telescope.nvim'
    },
    cmd = 'Octo',
    opts = {
      picker = 'telescope',
      enable_builtin = true,
    }
  }
}

require('lazy').setup({ defaults = { lazy = true }, install = { missing = true, colorscheme = { 'vscode' } }, spec = plugins, checker = { enabled = true } })

local configure_lsp = function(vim, lsp_configs)

  local default_float_opts = {
    anchor_bias = 'below',
    border = 'rounded',
    silent = true,
    focusable = false,
    relative = 'cursor'
  }

  local function wrap_lsp_handler(handler, opts)
    local original = handler
    return function()
      return original(vim.tbl_extend('force', default_float_opts, opts or {}))
    end
  end

  vim.lsp.buf.hover = wrap_lsp_handler(vim.lsp.buf.hover, { offset_x = 0, offset_y = 0 })
  vim.lsp.buf.signature_help = wrap_lsp_handler(vim.lsp.buf.signature_help, { offset_x = 0, offset_y = 1 })

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
        local chars = {}; for i = 32, 126 do table.insert(chars, string.char(i)) end
        client.server_capabilities.completionProvider.triggerCharacters = chars

        vim.lsp.completion.enable(true, client.id, bufnr, { autotrigger = true })
      end

      if vim.lsp.inline_completion and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlineCompletion, bufnr) then
        vim.lsp.inline_completion.enable(true, { bufnr = bufnr })
        vim.keymap.set('i', '<Tab>',
          function()
            if not vim.lsp.inline_completion.get() then
              return '<Tab>'
            end
          end, { expr = true, desc = 'Accept the current inline completion' })
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

      if vim.lsp.on_type_formatting  and client:supports_method(vim.lsp.protocol.Methods.textDocument_onTypeFormatting, bufnr) then
        vim.lsp.on_type_formatting.enable(true, { client_id = client.id })
      end

      if client:supports_method(vim.lsp.protocol.Methods.textDocument_willSaveWaitUntil) and
         client:supports_method(vim.lsp.protocol.Methods.textDocument_formatting) then

        vim.api.nvim_create_autocmd('BufWritePre', {
          buffer = bufnr,
          callback = function () vim.lsp.buf.format({ bufnr = bufnr, id = client.id, timeout_ms = 1000 }) end,
        })
      end

      if client:supports_method(vim.lsp.protocol.Methods.textDocument_signatureHelp, bufnr) then
        vim.api.nvim_create_autocmd( {'CursorHold', 'CursorHoldI'}, {
          buffer = bufnr,
          callback = function () vim.lsp.buf.signature_help() end,
        })
        vim.api.nvim_create_autocmd({'CursorMoved', 'CursorMovedI'}, {
          buffer = bufnr,
          callback = function() vim.lsp.buf.clear_references() end,
        })
      end

      if client:supports_method(vim.lsp.protocol.Methods.textDocument_hover, bufnr) then
        vim.api.nvim_create_autocmd( {'CursorHold', 'CursorHoldI'}, {
          buffer = bufnr,
          callback = function () vim.lsp.buf.hover() end,
        })
        vim.api.nvim_create_autocmd({'CursorMoved', 'CursorMovedI'}, {
          buffer = bufnr,
          callback = function() vim.lsp.buf.clear_references() end,
        })
      end

      if client:supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight, bufnr) then
        vim.api.nvim_create_autocmd({'CursorHold', 'CursorHoldI'}, {
          buffer = bufnr,
          callback = function () vim.lsp.buf.document_highlight() end,
        })
        vim.api.nvim_create_autocmd({'CursorMoved', 'CursorMovedI'}, {
          buffer = bufnr,
          callback = function() vim.lsp.buf.clear_references() end,
        })
      end
    end,
  })
  vim.api.nvim_create_autocmd('LspDetach',   { command = 'setl foldexpr<' })
  vim.api.nvim_create_autocmd('VimLeavePre', { callback = function () vim.iter(vim.lsp.get_clients()):each(function(client) client:stop() end) end, })


  vim.lsp.enable(vim.tbl_keys(lsp_configs))

  if vim.g.lsp_on_demands then
    vim.lsp.enable(vim.g.lsp_on_demands)
  end

end

configure_lsp(vim, lsp_configs)
