local vim = vim

local old = vim.opt.runtimepath:get()
vim.opt.runtimepath = vim.iter(old):filter(
  function(el)
    return vim.uv.fs_stat(vim.fs.normalize(el)) ~= nil
  end
):totable()

do
  local mason_bin = vim.fn.stdpath('data') .. '/mason/bin'
  if not (vim.env.PATH or ''):find(mason_bin, 1, true) then
    vim.env.PATH = mason_bin .. (vim.fn.has('win32') == 1 and ';' or ':') .. (vim.env.PATH or '')
  end
end

local treesitter_configs = { 'c', 'cpp', 'git_config', 'git_rebase', 'gitattributes', 'gitcommit', 'gitignore', 'rust',
  'yaml', 'markdown', 'markdown_inline', 'regex', 'bash', 'lua', 'cmake', 'json', 'json5', 'powershell', 'xml' }
local tools = { 'clang-format', 'codelldb' }

local lsp_configs = {
  ['clangd'] = {
    cmd = { 'clangd', '--background-index', '--clang-tidy', '--all-scopes-completion', '--pch-storage=memory', '--completion-style=detailed' },
    root_markers = { '.clangd', 'compile_commands.json', '.git', 'CMakeLists.txt' },
    filetypes = { 'c', 'cpp', 'objc', 'objcpp', 'cuda', 'proto' },
    workspace_required = false,
  },
  ['cmake-language-server'] = {
    cmd = { 'cmake-language-server' },
    filetypes = { 'cmake' },
    root_markers = { 'CMakePresets.json', 'CTestConfig.cmake', '.git', 'build', 'cmake' },
    workspace_required = false,
    init_options = {
      buildDirectory = 'build',
    },
  },
  ['lua-language-server'] = {
    cmd = { 'lua-language-server' },
    filetypes = { 'lua' },
    root_markers = { '.luarc.json', '.luarc.jsonc', '.git', 'lua' },
    workspace_required = false,
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
  ['marksman'] = {
    cmd = { 'marksman' },
    root_markers = { '.marksman.toml', '.git', '*.md' },
    filetypes = { 'markdown' },
    workspace_required = false,
  },
  ['yaml-language-server'] = {
    cmd = { 'yaml-language-server', '--stdio' },
    filetypes = { 'yaml', 'yaml.docker-compose', 'yaml.gitlab', 'yaml.helm-values' },
    root_markers = { '.git' },
    workspace_required = false,
    settings = {
      redhat = { telemetry = { enabled = false } },
      yaml = { format = { enable = true } },
    },
    on_init = function(client)
      client.server_capabilities.documentFormattingProvider = true
    end,
  },
  ['copilot-language-server'] = {
    cmd = { 'copilot-language-server', '--stdio' },
    root_markers = { '.git' },
    workspace_required = false,
    init_options = {
      editorInfo = {
        name = 'Neovim',
        version = tostring(vim.version()),
      },
      editorPluginInfo = {
        name = 'Neovim',
        version = tostring(vim.version()),
      },
    },
    settings = {
      telemetry = {
        telemetryLevel = 'all',
      },
    },
    on_attach = function(client, bufnr)
      vim.api.nvim_buf_create_user_command(bufnr, 'LspCopilotSignIn', function()
        client:request(
          'signIn',
          vim.empty_dict(),
          function(err, result)
            if err then
              vim.notify(err.message, vim.log.levels.ERROR)
              return
            end
            if result.command then
              local code = result.userCode
              local command = result.command
              vim.fn.setreg('+', code)
              vim.fn.setreg('*', code)
              local continue = vim.fn.confirm(
                'Copied your one-time code to clipboard.\n' .. 'Open the browser to complete the sign-in process?',
                '&Yes\n&No'
              )
              if continue == 1 then
                client:exec_cmd(command, { bufnr = bufnr }, function(cmd_err, cmd_result)
                  if cmd_err then
                    vim.notify(cmd_err.message, vim.log.levels.ERROR)
                    return
                  end
                  if cmd_result.status == 'OK' then
                    vim.notify('Signed in as ' .. cmd_result.user .. '.')
                  end
                end)
              end
            end

            if result.status == 'PromptUserDeviceFlow' then
              vim.notify('Enter your one-time code ' .. result.userCode .. ' in ' .. result.verificationUri)
            elseif result.status == 'AlreadySignedIn' then
              vim.notify('Already signed in as ' .. result.user .. '.')
            end
          end
        )
      end, { desc = 'Sign in Copilot with GitHub' })
    end,
  },
}

local configure_defaults = function(vim)
  vim.g.mapleader = " "
  vim.g.maplocalleader = "\\"

  vim.g.builtin_autocompletion = true

  vim.opt.cmdheight = 1

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
  vim.opt.signcolumn = "yes:2"
  vim.opt.isfname:append("@-@")

  vim.opt.keymodel = "startsel,stopsel"

  vim.opt.undofile = true
  vim.opt.undodir = vim.fn.expand("~/.undodir")

  vim.opt.number = true

  vim.opt.termguicolors = true

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
    local normal_bg = (vim.api.nvim_get_hl(0, { id = vim.api.nvim_get_hl_id_by_name("Normal") }) or {}).bg
    if normal_bg then
      vim.g.neovide_title_background_color = string.format("%x", normal_bg)
    end
    vim.g.neovide_show_border = true
    vim.g.neovide_theme = 'auto'
    vim.g.neovide_refresh_rate = 144
    vim.g.experimental_layer_grouping = true
    vim.g.neovide_refresh_rate_idle = 1
    vim.g.neovide_fullscreen = false
    vim.g.neovide_macos_simple_fullscreen = true
    vim.g.neovide_floating_shadow = false
    vim.g.neovide_light_radius = 0
  end

  vim.opt.ignorecase = true
  vim.opt.smartcase = true

  vim.opt.wildoptions = "pum,fuzzy,exacttext"
  vim.opt.wildmode = "longest:full,full"

  vim.opt.statusline = "%f %m%r%h%w %= %{v:lua.Lsp_progress()} %l:%c %p%%"

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

vim.schedule(function()
  require('vim._core.ui2').enable({
    enable = true,
    msg = {
      targets = {
        [''] = 'msg',
        empty = 'cmd',
        bufwrite = 'msg',
        confirm = 'cmd',
        emsg = 'pager',
        echo = 'msg',
        echomsg = 'msg',
        echoerr = 'pager',
        completion = 'cmd',
        list_cmd = 'pager',
        lua_error = 'pager',
        lua_print = 'msg',
        progress = 'pager',
        rpc_error = 'pager',
        quickfix = 'pager',
        search_cmd = 'cmd',
        search_count = 'cmd',
        shell_cmd = 'pager',
        shell_err = 'pager',
        shell_out = 'pager',
        shell_ret = 'msg',
        undo = 'msg',
        verbose = 'pager',
        wildlist = 'cmd',
        wmsg = 'msg',
        typed_cmd = 'cmd',
      },
      cmd = {
        height = 0.5,
      },
      dialog = {
        height = 0.5,
      },
      msg = {
        height = 0.3,
        timeout = 5000,
      },
      pager = {
        height = 0.5,
      },
    },
  })
end)

local plugins = {
  { 'nvim-lua/plenary.nvim', lazy = false },
  {
    'wsdjeg/rooter.nvim',
    lazy = false,
    opts = {
      enable_cache = true,
      project_non_root = 'current',
      root_pattern = { '.git', 'Cargo.toml', 'README.md', '.cargo' },
    },
  },
  {
    'Mofiqul/vscode.nvim',
    lazy = false,
    opts = {
      transparent = true,
      italic_comments = false,
      italic_inlayhints = false,
      terminal_colors = true
    },
    config = function()
      vim.cmd("colorscheme vscode")
    end,
  },
  {
    'f-person/auto-dark-mode.nvim',
    lazy = false,
    opts = {
      update_interval = 1000,
      set_dark_mode = function()
        vim.api.nvim_set_option_value("background", "dark", {})
        vim.cmd("colorscheme vscode")
      end,
      set_light_mode = function()
        vim.api.nvim_set_option_value("background", "light", {})
        vim.cmd("colorscheme vscode")
      end,
    },
  },
  {
    'stevearc/oil.nvim',
    lazy = false,
    keys = {
      { 'fe', mode = { 'n' }, function() require('oil').open() end, desc = 'Open Oil file explorer' },
    },
    opts = {
      default_file_explorer = true,
      delete_to_trash = true,
      watch_for_changes = true,
      columns = { 'permissions', 'size', 'mtime' },
      win_options = { signcolumn = 'yes:2' },
      view_options = { show_hidden = true },
    },
  },
  {
    'nvim-treesitter/nvim-treesitter',
    version = 'main',
    lazy = false,
    build = ':TSUpdate',
    config = function()
      vim.treesitter.language.register('markdown', 'octo')
      vim.treesitter.language.register('xml', 'msbuild')

      local ts = require('nvim-treesitter')
      ts.install(treesitter_configs)

      local group = vim.api.nvim_create_augroup('TreesitterSetup', { clear = true })

      vim.api.nvim_create_autocmd('FileType', {
        group = group,
        desc = 'Enable treesitter highlighting and indentation',
        callback = function(event)
          local lang = vim.treesitter.language.get_lang(event.match) or event.match

          if not vim.tbl_contains(treesitter_configs, lang) then
            return
          end

          local buf = event.buf
          pcall(vim.treesitter.start, buf, lang)
          vim.bo[buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
          ts.install({ lang })
        end,
      })
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter-context',
    events = { 'BufRead' },
    opts = {},
  },
  {
    'mason-org/mason.nvim',
    cmd = { 'Mason', 'MasonInstall', 'MasonUninstall', 'MasonUpdate', 'MasonLog' },
    events = { 'VimEnter' },
    opts = {},
  },
  {
    'WhoIsSethDaniel/mason-tool-installer.nvim',
    cmd = { 'Mason', 'MasonInstall', 'MasonUninstall', 'MasonUpdate', 'MasonLog', 'MasonToolsClean', 'MasonToolsInstall', 'MasonToolsUpdate', 'MasonToolsInstallSync', 'MasonToolsUpdateSync' },
    events = { 'VimEnter' },
    build = ':MasonToolsInstall',
    config = function()
      require('mason').setup()
      local all_tools = vim.list_extend(vim.tbl_keys(lsp_configs), tools)
      for _, tool in pairs(all_tools) do
        if vim.fn.executable(tool) == 0 then
          print("Installing tool: " .. tool)
          vim.cmd("MasonInstall " .. tool)
        end
      end
    end,
  },
  {
    'windwp/nvim-autopairs',
    events = { 'InsertEnter' },
    opts = { map_bs = false },
  },
  {
    'kylechui/nvim-surround',
    events = { 'UIEnter' },
    opts = {},
  },
  {
    'qwavies/smart-backspace.nvim',
    events = { 'CmdlineEnter' },
    opts = {},
  },
  {
    'lewis6991/gitsigns.nvim',
    events = { 'BufRead' },
    opts = {
      signs_staged_enable = true,
      signcolumn          = true,
      numhl               = true,
      linehl              = false,
      word_diff           = true,
      watch_gitdir        = { follow_files = true },
      auto_attach         = true,
      attach_to_untracked = false,
      preview_config      = {
        style = 'minimal',
        relative = 'cursor',
        row = 0,
        col = 1
      },
    },
  },
  {
    'dlyongemallo/diffview.nvim',
    cmd = { 'DiffviewOpen', 'DiffviewFileHistory' },
    config = function()
      local actions = require('diffview.actions')
      require('diffview').setup({
        enhanced_diff_hl = true,
        use_icons = false,
        show_help_hints = true,
        watch_index = true,
        view = {
          default = { layout = 'diff2_horizontal' },
          merge_tool = {
            layout = 'diff4_mixed',
            disable_diagnostics = true,
            winbar_info = true
          }
        },
        hooks = {
          diff_buf_win_enter = function(_)
            vim.opt_local.foldenable = false
          end
        },
        keymaps = {
          file_history_panel = {
            { "n", "<cr>", actions.focus_entry, { desc = "Open and focus the diff for the selected entry." } }
          },
        }
      })
    end,
  },
  {
    'NeogitOrg/neogit',
    events = { 'VimEnter' },
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
      },
      diff_viewer = 'diffview',
      signs = {
        hunk = { "+", "-" },
        item = { "+", "-" },
        section = { "+", "-" },
      },
      commit_editor = {
        kind = "tab",
        show_staged_diff = true,
        spell_check = true
      },
      sections = {
        untracked = { folded = true },
      },
    }
  },
  {
    'pwntester/octo.nvim',
    cmd = 'Octo',
    opts = {
      picker = 'default',
      enable_builtin = true,
      use_timeline_icons = false,
      runs = {
        icons = {
          pending = "[Pending]",
          in_progress = "[In Progress]",
          failed = "[Failed]",
          succeeded = "",
          skipped = "[Skipped]",
          cancelled = "[Cancelled]",
        },
      },
    },
  },
  {
    'kdheepak/lazygit.nvim',
    cmd = { 'LazyGit', 'LazyGitConfig', 'LazyGitCurrentFile', 'LazyGitFilter', 'LazyGitFilterCurrentFile' },
    keys = {
      { "<leader>lg", "<cmd>LazyGit<cr>", desc = "LazyGit" }
    },
  },
}

vim.api.nvim_create_autocmd("PackChanged", {
  callback = function(ev)
    if ev.data.kind ~= "install" and ev.data.kind ~= "update" then return end

    local build = ev.data.spec.data and ev.data.spec.data.build
    if not build then return end

    local name = ev.data.spec.name
    if build:sub(1, 1) == ':' then
      vim.cmd('packadd ' .. name)
      local ok, err = pcall(vim.cmd, build:sub(2))
      if not ok then
        vim.notify(('Build failed for %s: %s'):format(name, err), vim.log.levels.ERROR)
      end
    else
      local result = vim.system({ "sh", "-c", build }, {
        cwd = ev.data.path,
        text = true,
      }):wait()
      if result.code ~= 0 then
        vim.notify(
          ('Build failed for %s (exit %d)\n%s'):format(name, result.code, result.stderr or ''),
          vim.log.levels.ERROR
        )
      end
    end
  end,
})

local pack_fields = { src = true, name = true, version = true }
local plugin_meta = {}
local eager_specs = {}
local deferred_specs = {}
local managed_names = {}

for _, spec in ipairs(plugins) do
  local url = spec[1]
  spec[1] = nil
  if not url:match('^https?://') then
    url = 'https://github.com/' .. url
  end
  spec.src = url

  local meta = {}
  for k, v in pairs(spec) do
    if not pack_fields[k] then
      meta[k] = v
      spec[k] = nil
    end
  end
  if next(meta) then plugin_meta[spec.src] = meta end

  spec.name = spec.name or spec.src:gsub('%.git$', ''):match('[^/]+$')
  managed_names[spec.name] = true

  if meta.lazy == false then
    table.insert(eager_specs, spec)
  else
    table.insert(deferred_specs, spec)
  end
end

local function setup_plugin(name, meta)
  if not meta then return end
  if not meta.config and not meta.opts then return end
  local opts = meta.opts or {}
  local mod_name = name:gsub('%.nvim$', ''):gsub('%.lua$', ''):gsub('^nvim%-', ''):gsub('^nvim_', '')
  local ok, mod = pcall(require, mod_name)
  if not ok then
    ok, mod = pcall(require, name)
  end
  if ok and type(mod) == 'table' and mod.setup then
    mod.setup(opts)
  end
  if meta.config then
    meta.config(ok and mod or nil, opts)
  end
end

local loaded_plugins = {}

local function load_and_setup(spec, meta)
  if loaded_plugins[spec.src] then return end
  loaded_plugins[spec.src] = true
  vim.pack.add({ spec }, { load = true })
  setup_plugin(spec.name, meta)
end

-- Load eager plugins and run their setup immediately so colorschemes / UI
-- plugins don't flicker through a VimEnter detour.
vim.pack.add(eager_specs)
for _, spec in ipairs(eager_specs) do
  loaded_plugins[spec.src] = true
  setup_plugin(spec.name, plugin_meta[spec.src])
end

vim.pack.add(deferred_specs, { load = false })

for _, spec in ipairs(deferred_specs) do
  local m = plugin_meta[spec.src] or {}
  local function trigger() load_and_setup(spec, m) end
  local has_trigger = m.events or m.cmd or m.ft or m.keys

  if m.events then
    vim.api.nvim_create_autocmd(m.events, { once = true, callback = trigger })
  end

  if m.cmd then
    local cmds = type(m.cmd) == 'string' and { m.cmd } or m.cmd
    for _, cmd in ipairs(cmds) do
      vim.api.nvim_create_user_command(cmd, function(args)
        vim.api.nvim_del_user_command(cmd)
        local ok, err = pcall(load_and_setup, spec, m)
        if not ok then
          vim.notify(('Failed to load %s: %s'):format(spec.name, err), vim.log.levels.ERROR)
          return
        end
        vim.api.nvim_cmd({
          cmd = cmd,
          args = args.fargs,
          bang = args.bang,
          range = args.range > 0 and { args.line1, args.line2 } or nil,
          mods = args.smods,
        }, {})
      end, { nargs = '*', bang = true, range = true })
    end
  end

  if m.ft then
    local fts = type(m.ft) == 'string' and { m.ft } or m.ft
    vim.api.nvim_create_autocmd('FileType', { pattern = fts, once = true, callback = trigger })
  end

  if m.keys then
    for _, keyspec in ipairs(m.keys) do
      local lhs = keyspec[1]
      local modes = keyspec.mode or { 'n' }
      if type(modes) == 'string' then modes = { modes } end
      local rhs = keyspec[2]
      local key_opts = { desc = keyspec.desc, nowait = keyspec.nowait }

      if type(rhs) == 'function' then
        local fn = rhs
        vim.keymap.set(modes, lhs, function()
          load_and_setup(spec, m)
          fn()
        end, key_opts)
      elseif type(rhs) == 'string' then
        vim.keymap.set(modes, lhs, function()
          load_and_setup(spec, m)
          local keys = vim.api.nvim_replace_termcodes(rhs, true, true, true)
          vim.api.nvim_feedkeys(keys, 'mt', false)
        end, key_opts)
      end
    end
  end

  if not has_trigger and (m.config or m.opts) then
    vim.schedule(trigger)
  end
end

vim.api.nvim_create_user_command('PackUpdate', function(args)
  vim.pack.update(#args.fargs > 0 and args.fargs or nil)
end, {
  nargs = '*',
  complete = function()
    return vim.iter(vim.pack.get())
        :filter(function(p) return managed_names[p.spec.name] end)
        :map(function(p) return p.spec.name end)
        :totable()
  end,
})

vim.api.nvim_create_user_command('PackClean', function()
  local orphans = vim.iter(vim.pack.get())
      :filter(function(p) return not managed_names[p.spec.name] end)
      :map(function(p) return p.spec.name end)
      :totable()
  if #orphans == 0 then
    vim.notify('No orphaned plugins to remove', vim.log.levels.INFO)
    return
  end
  vim.pack.del(orphans)
end, {})

vim.api.nvim_create_user_command('UpdateAll', function()
  vim.cmd('PackClean')

  local pre_wins = vim.api.nvim_tabpage_list_wins(0)
  vim.cmd('PackUpdate')
  local post_wins = vim.api.nvim_tabpage_list_wins(0)

  local new_wins = vim.tbl_filter(function(w)
    return not vim.tbl_contains(pre_wins, w)
  end, post_wins)

  local function continue()
    vim.schedule(function()
      vim.cmd('TSUpdate')
      vim.cmd('Mason')
    end)
  end

  if #new_wins > 0 then
    vim.api.nvim_create_autocmd('WinClosed', {
      pattern = tostring(new_wins[1]),
      once = true,
      callback = continue,
    })
  else
    continue()
  end
end, { desc = 'Clean packages, update plugins, and update Mason tools' })

local configure_global_keymaps = function()
  local opts = { noremap = true, silent = true }
  local set = vim.keymap.set
  set("i", "<S-Tab>", "<C-\\><C-N><<<C-\\><C-N>^i", opts)
end

local configure_window_management = function()
  local opts = { noremap = true, silent = true }
  local set = vim.keymap.set

  local collapsed_width = 3

  local window_state = {
    maximized_win = nil,
    saved_widths = {},
    label_bufs = {},
    collapsed_wins = {},
  }

  vim.api.nvim_set_hl(0, 'WindowDimmed', { fg = '#666666', bg = 'NONE' })
  vim.api.nvim_set_hl(0, 'WindowDimmedBg', { bg = '#1a1a1a', fg = '#666666' })

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
    vim.bo[new_buf].buftype = ''
    vim.bo[new_buf].buflisted = true
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


local configure_lsp = function(vim, lsp_configs)
  local default_float_opts = {
    anchor_bias = 'below',
    border = 'rounded',
    silent = true,
    focusable = false,
    relative = 'cursor'
  }

  local function wrap_lsp_handler(handler, defaults)
    local original = handler
    return function(caller_opts)
      return original(vim.tbl_extend('force', default_float_opts, defaults or {}, caller_opts or {}))
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
    },
    root_markers = { '.git' },
    workspace_required = false,
  })

  for name, config in pairs(lsp_configs) do
    vim.lsp.config(name, config)
  end

  vim.api.nvim_create_autocmd('LspAttach', {
    callback = function(args)
      local client = vim.lsp.get_client_by_id(args.data.client_id)
      local bufnr = args.buf

      if client:supports_method(vim.lsp.protocol.Methods.textDocument_completion, bufnr) then
        vim.lsp.completion.enable(true, client.id, bufnr, { autotrigger = true })
      end

      if vim.lsp.inline_completion and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlineCompletion, bufnr) then
        vim.lsp.inline_completion.enable(true, { bufnr = bufnr })
      end

      if client:supports_method(vim.lsp.protocol.Methods.textDocument_documentColor, bufnr) then
        vim.lsp.document_color.enable(true, { bufnr = bufnr, style = 'virtual' })
      end

      if client:supports_method(vim.lsp.protocol.Methods.textDocument_semanticTokens_full, bufnr) then
        vim.lsp.semantic_tokens.enable(true, { bufnr = bufnr })
      end

      if vim.lsp.on_type_formatting and client:supports_method(vim.lsp.protocol.Methods.textDocument_onTypeFormatting, bufnr) then
        vim.lsp.on_type_formatting.enable(true, { client_id = client.id })
      end

      local group = vim.api.nvim_create_augroup("UserLsp_" .. bufnr, { clear = true })

      local clients = vim.lsp.get_clients({ bufnr = bufnr })
      local has_format, has_highlight, has_hover, has_sig = false, false, false, false
      for _, c in ipairs(clients) do
        has_format = has_format or c:supports_method(vim.lsp.protocol.Methods.textDocument_formatting, bufnr)
        has_highlight = has_highlight or
            c:supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight, bufnr)
        has_hover = has_hover or c:supports_method(vim.lsp.protocol.Methods.textDocument_hover, bufnr)
        has_sig = has_sig or c:supports_method(vim.lsp.protocol.Methods.textDocument_signatureHelp, bufnr)
      end

      if has_format then
        vim.api.nvim_create_autocmd('BufWritePre', {
          group = group,
          buffer = bufnr,
          callback = function() vim.lsp.buf.format({ bufnr = bufnr, timeout_ms = 1000 }) end,
        })
      end

      if has_highlight or has_hover or has_sig then
        vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
          group = group,
          buffer = bufnr,
          callback = function()
            if has_highlight then vim.lsp.buf.document_highlight() end
            local mode = vim.api.nvim_get_mode().mode
            if has_sig and (mode == 'i' or mode == 'ic') then
              vim.lsp.buf.signature_help()
            elseif has_hover and mode == 'n' then
              vim.lsp.buf.hover()
            end
          end,
        })
        vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, {
          group = group,
          buffer = bufnr,
          callback = function() vim.lsp.buf.clear_references() end,
        })
      end

      vim.keymap.set('i', '<Tab>', function()
        if vim.lsp.inline_completion and vim.lsp.inline_completion.get() then
          vim.lsp.inline_completion.accept()
          return ''
        end
        return '<Tab>'
      end, { expr = true, buffer = bufnr, desc = 'Accept inline completion or Tab' })

      local map = function(lhs, fn, desc)
        vim.keymap.set('n', lhs, fn, { buffer = bufnr, desc = desc })
      end
      map('gd', vim.lsp.buf.definition, 'Goto Definition')
      map('gr', vim.lsp.buf.references, 'References')
      map('gI', vim.lsp.buf.implementation, 'Goto Implementation')
      map('gy', vim.lsp.buf.type_definition, 'Goto Type Definition')
      map('gai', vim.lsp.buf.incoming_calls, 'Incoming Calls')
      map('gao', vim.lsp.buf.outgoing_calls, 'Outgoing Calls')
      map('<leader>ss', vim.lsp.buf.document_symbol, 'Document Symbols')
      map('<leader>sS', vim.lsp.buf.workspace_symbol, 'Workspace Symbols')
    end,
  })
  local progress_msg = ''
  local progress_timer = nil

  function Lsp_progress()
    return progress_msg
  end

  vim.api.nvim_create_autocmd('LspProgress', {
    callback = function(ev)
      local data = ev.data
      local client = vim.lsp.get_client_by_id(data.client_id)
      local name = client and client.name or ''
      local val = data.params and data.params.value or {}
      local kind = val.kind
      if kind == 'end' then
        progress_msg = ''
      else
        local title = val.title or ''
        local message = val.message or ''
        local pct = val.percentage and (val.percentage .. '%%') or ''
        progress_msg = table.concat(vim.tbl_filter(function(s) return s ~= '' end, { name, title, message, pct }), ' ')
      end
      vim.cmd.redrawstatus()
      if progress_timer then
        vim.fn.timer_stop(progress_timer)
      end
      progress_timer = vim.fn.timer_start(3000, function()
        progress_msg = ''
        progress_timer = nil
        vim.schedule(function() vim.cmd.redrawstatus() end)
      end)
    end,
  })

  vim.api.nvim_create_autocmd('VimLeavePre',
    { callback = function() vim.iter(vim.lsp.get_clients()):each(function(client) client:stop() end) end, })

  vim.lsp.enable(vim.tbl_keys(lsp_configs))

  if vim.g.lsp_on_demands then
    vim.lsp.enable(vim.g.lsp_on_demands)
  end
end

configure_defaults(vim)
configure_global_keymaps()
configure_window_management()
configure_lsp(vim, lsp_configs)
