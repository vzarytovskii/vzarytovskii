local vim = vim

local socket_dir = os.getenv("XDG_RUNTIME_DIR") or os.getenv("TMPDIR") or "/tmp"
local socket_path = socket_dir .. "/nvim_default.sock"

-- A connectable socket means a session is already running, so this instance is
-- only a launcher: forward its file arguments over RPC and hand the UI over on
-- UIEnter. Otherwise become the session -- nvim only ever autostarts a server
-- on a random path, so the shared one has to be created explicitly.
local probed, session = pcall(vim.fn.sockconnect, 'pipe', socket_path, { rpc = true })
local joining = probed and session ~= 0
if joining then
  local opens = vim.tbl_map(function(arg)
    return 'drop ' .. vim.fn.fnameescape(vim.fn.fnamemodify(arg, ':p'))
  end, vim.fn.argv())
  if #opens > 0 then
    pcall(vim.rpcrequest, session, 'nvim_exec2', table.concat(opens, ' | '), vim.empty_dict())
  end
  pcall(vim.fn.chanclose, session)
else
  os.remove(socket_path)
  vim.fn.serverstart(socket_path)
end


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

-- Project-root markers shared by automatic cwd detection (VimEnter/BufEnter) and
-- the fzf pickers. '.cargo' is intentionally omitted: the global Cargo home
-- (~/.cargo) would otherwise make every non-project file resolve its root to $HOME.
local project_root_markers = { '.git', 'Cargo.toml', 'Cargo.lock', 'CMakeLists.txt' }

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
}

local configure_defaults = function(vim)
  vim.opt.guicursor = "n-v-c:block,i-ci-ve:ver90"

  vim.g.mapleader = " "
  vim.g.maplocalleader = "\\"

  vim.wo.fillchars = 'eob: '

  vim.opt.grepformat = "%f:%l:%c:%m,%f"

  vim.opt.cmdheight = 1
  vim.opt.showmode = false

  vim.opt.updatetime = 500
  vim.opt.ttimeoutlen = 10

  vim.opt.hidden = true

  vim.opt.encoding = "utf-8"
  vim.opt.fileencodings = 'utf-8,utf-16,utf-32,ucs-bom,default,latin'

  vim.opt.fileformat = 'unix'
  vim.opt.fileformats = 'unix,dos,mac'

  vim.opt.pumblend = 0
  vim.opt.pumborder = 'rounded'
  vim.opt.pumheight = 7
  vim.opt.pummaxwidth = 180
  vim.opt.pumwidth = 80

  vim.g.builtin_autocompletion = true
  vim.opt.autocomplete = true
  vim.opt.complete = 'o,F,.,i,d,w,b,u,t'
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
  vim.opt.signcolumn = "yes"
  vim.opt.isfname:append("@-@")
  vim.opt.selectmode = ""
  vim.opt.keymodel = "startsel"

  vim.opt.undofile = true
  local state_dir = vim.fn.stdpath('state')
  local undo_dir = state_dir .. '/undo//'
  local backup_dir = state_dir .. '/backup//'
  local swap_dir = state_dir .. '/swap//'

  vim.fn.mkdir(state_dir .. '/undo', 'p')
  vim.fn.mkdir(state_dir .. '/backup', 'p')
  vim.fn.mkdir(state_dir .. '/swap', 'p')

  vim.opt.undodir = undo_dir
  vim.opt.backup = true
  vim.opt.writebackup = true
  vim.opt.backupdir = backup_dir
  vim.opt.swapfile = true
  vim.opt.directory = swap_dir

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

  vim.opt.statusline = "%{v:lua.Statusline_mode()} %f %m%r%h%w %= %{v:lua.Lsp_progress()} %l:%c %p%%"

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
        empty = 'msg',
        bufwrite = 'cmd',
        confirm = 'cmd',
        emsg = 'msg',
        echo = 'msg',
        echomsg = 'msg',
        echoerr = 'msg',
        completion = 'cmd',
        list_cmd = 'pager',
        lua_error = 'msg',
        lua_print = 'msg',
        progress = 'msg',
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
    'f-person/auto-dark-mode.nvim',
    lazy = false,
    opts = {
      update_interval = 1000,
      set_dark_mode = function()
        vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
        vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
        vim.api.nvim_set_option_value("background", "dark", {})
--        vim.cmd("colorscheme default")
      end,
      set_light_mode = function()
        vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
        vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
        vim.api.nvim_set_option_value("background", "light", {})
--        vim.cmd("colorscheme default")
      end,
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
    'dlyongemallo/diffview-plus.nvim',
    cmd = { 'DiffviewOpen', 'DiffViewToggle', 'DiffviewFileHistory', 'DiffviewDiffFiles', 'DiffviewLog' },
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
    events = { 'VimEnter' },
    cmd = 'Octo',
    opts = {
      picker = 'default',
      enable_builtin = true,
      use_timeline_icons = false,
      file_panel = {
        icons = false
      },
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
}

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
  -- Expose build hooks on the pack spec so the PackChanged handler can run them.
  if meta.build then spec.data = { build = meta.build } end

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
  if meta.config then
    meta.config(ok and mod or nil, opts)
  elseif ok and type(mod) == 'table' and mod.setup then
    mod.setup(opts)
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
          vim.notify(('Failed to load %s: %s'):format(spec.name, err), vim.log.levels
            .ERROR)
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

local configure_global_keymaps = function(vim)
  local opts = { noremap = true, silent = true }
  local set = vim.keymap.set

  set("i", "<S-Tab>", "<C-\\><C-N><<<C-\\><C-N>^i", opts)
  set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

  set('n', '<leader>eD', '<cmd>silent detach<cr>', { desc = "Detach from current session" })

  set('n', '<C-a>', '^', { desc = "Beginning of line" })
  set('n', '<C-e>', '$', { desc = "End of line" })
  set('n', '<C-k>', 'd$', { desc = "Kill the line till the end of line" })

  set('i', '<C-a>', '<C-o>^', { desc = "Beginning of line" })
  set('i', '<C-e>', '<C-o>$', { desc = "End of line" })
  set('i', '<C-k>', '<C-o>d$', { desc = "Kill the line till the end of line" })

  set('v', '<C-a>', '^', { desc = "Beginning of line" })
  set('v', '<C-e>', '$', { desc = "End of line" })
  set('v', '<C-k>', 'd$', { desc = "Kill the line till the end of line" })


  set("n", "<leader>gl", "<cmd>LazyGit<cr>", { desc = "LazyGit" })
  set("n", "<leader>gs", "<cmd>Neogit<cr>", { desc = "Neogit" })
  set("n", "<leader>ff", "<cmd>FzfFiles<cr>", { desc = "Find files (fd + fzf)" })
  set("n", "<leader>lf", "<cmd>FzfFiles<cr>", { desc = "Find files (fd + fzf)" })
  set("n", "<leader>fr", "<cmd>FzfRecents<cr>", { desc = "Recent files (fzf)" })
  set("n", "<leader>lr", "<cmd>FzfRecents<cr>", { desc = "Recent files (fzf)" })
  set("n", "<leader>fR", "<cmd>FzfRecentDirs<cr>", { desc = "Recent folders (fzf)" })
  set("n", "<leader>lR", "<cmd>FzfRecentDirs<cr>", { desc = "Recent folders (fzf)" })
  set("n", "<leader>fg", "<cmd>FzfGrep<cr>", { desc = "Grep content (rg + fzf)" })
  set("n", "<leader>fG", "<cmd>FzfGrepDir<cr>", { desc = "Grep in current dir (rg + fzf)" })
  set("n", "<leader>flg", "<cmd>FzfLiveGrep<cr>", { desc = "Live grep with preview" })
  set("n", "<leader>lb", "<cmd>FzfBuffers<cr>", { desc = "Buffer list (fzf)" })
  set("n", "<leader>bK", function()
    local wins = vim.tbl_filter(function(w)
      return vim.api.nvim_win_get_config(w).relative == ''
    end, vim.api.nvim_tabpage_list_wins(0))
    if #wins > 1 then
      vim.api.nvim_win_close(0, false)
    else
      local force = vim.bo[vim.api.nvim_get_current_buf()].buftype == 'terminal'
      vim.cmd('bdelete' .. (force and '!' or ''))
    end
  end, { desc = "Close window or kill buffer" })
  set("n", "<leader>nb", "<cmd>enew<cr>", { desc = "New buffer" })
  set("n", "<leader>nt", "<cmd>TermNext<cr>", { desc = "Next idle terminal or create new" })
  set("n", "<leader>nT", "<cmd>TermNew<cr>", { desc = "Create new terminal" })
  set("n", "<leader>na", "<cmd>AgentTermNew<cr>", { desc = "Create new agent terminal" })
  set("n", "<leader>nA", "<cmd>AgentTermNewWithFlags<cr>", { desc = "Create new agent terminal with custom command" })
  set("n", "<leader>lt", "<cmd>FzfTerminals<cr>", { desc = "Terminal list (fzf)" })
  set("n", "<leader>lA", "<cmd>FzfAgentTerminals<cr>", { desc = "Agent terminal list (fzf)" })
  set("t", "<C-Space>n", "<C-\\><C-n><cmd>TermNext<cr>", { desc = "Next idle terminal or create new" })
  set("t", "<C-Space>N", "<C-\\><C-n><cmd>TermNew<cr>", { desc = "Create new terminal" })
  set("t", "<C-Space>l", "<C-\\><C-n><cmd>FzfTerminals<cr>", { desc = "Terminal list (fzf)" })
  set("t", "<C-Space>b", "<C-\\><C-n><cmd>FzfBuffers<cr>", { desc = "Buffer list (fzf)" })
  set("t", "<C-Space>f", "<C-\\><C-n><cmd>FzfFiles<cr>", { desc = "Find files (fd + fzf)" })
  set({ "n", "i", "v" }, "<D-p>", "<cmd>FzfFiles<cr>", { desc = "Find files (Cmd-P)" })

  local function ace_window()
    local wins = vim.tbl_filter(function(w)
      local cfg = vim.api.nvim_win_get_config(w)
      return cfg.relative == ''
    end, vim.api.nvim_tabpage_list_wins(0))

    if #wins <= 1 then return end

    local cur = vim.api.nvim_get_current_win()

    if #wins == 2 then
      local target = wins[1] == cur and wins[2] or wins[1]
      vim.api.nvim_set_current_win(target)
      return
    end

    local labels = 'asdfjkl;ghqweruiop'
    local overlays = {}

    for i, w in ipairs(wins) do
      local label = labels:sub(i, i)
      if label == '' then break end
      local buf = vim.api.nvim_create_buf(false, true)
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, { ' ' .. label .. ' ' })
      local width = vim.api.nvim_win_get_width(w)
      local height = vim.api.nvim_win_get_height(w)
      local ow = vim.api.nvim_open_win(buf, false, {
        relative = 'win',
        win = w,
        width = 3,
        height = 1,
        row = math.floor(height / 2),
        col = math.floor(width / 2) - 1,
        style = 'minimal',
        border = 'rounded',
        focusable = false,
        zindex = 200,
      })
      vim.api.nvim_set_hl(0, 'AceWindowLabel', { fg = '#ff5f00', bg = '#1a1a1a', bold = true })
      vim.api.nvim_set_option_value('winhighlight', 'Normal:AceWindowLabel', { win = ow })
      table.insert(overlays, { win = ow, buf = buf, label = label, target = w })
    end

    vim.cmd('redraw')
    local ok, char = pcall(vim.fn.getcharstr)

    for _, o in ipairs(overlays) do
      if vim.api.nvim_win_is_valid(o.win) then vim.api.nvim_win_close(o.win, true) end
      if vim.api.nvim_buf_is_valid(o.buf) then vim.api.nvim_buf_delete(o.buf, { force = true }) end
    end

    if not ok then return end
    for _, o in ipairs(overlays) do
      if char == o.label and vim.api.nvim_win_is_valid(o.target) then
        vim.api.nvim_set_current_win(o.target)
        return
      end
    end
  end

  set("n", "<leader>w", ace_window, { desc = "Ace window switch" })

  local function show_help()
    local leader_items = {}
    local other_items = {}

    for _, mode in ipairs({ 'n', 'v', 'i', 't' }) do
      for _, km in ipairs(vim.api.nvim_get_keymap(mode)) do
        if km.desc and km.desc ~= '' then
          local lhs = km.lhs
          local entry = string.format('%-20s [%s] %s', lhs, mode, km.desc)
          if lhs:match(' ') then
            table.insert(leader_items, entry)
          else
            table.insert(other_items, entry)
          end
        end
      end
    end

    table.sort(leader_items)
    table.sort(other_items)

    local items = vim.list_extend({ 'LEADER BINDINGS:', '─────────────────' }, leader_items)
    items = vim.list_extend(items, { '', 'OTHER BINDINGS:', '────────────────' })
    items = vim.list_extend(items, other_items)

    if #leader_items == 0 and #other_items == 0 then
      vim.notify('No keybindings found', vim.log.levels.INFO)
      return
    end

    local height = math.min(#items + 2, vim.o.lines - 4)
    local width = math.min(80, vim.o.columns - 4)

    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, items)
    vim.bo[buf].modifiable = false
    vim.bo[buf].buftype = 'nofile'

    local win = vim.api.nvim_open_win(buf, true, {
      relative = 'editor',
      width = width,
      height = height,
      row = math.floor((vim.o.lines - height) / 2),
      col = math.floor((vim.o.columns - width) / 2),
      style = 'minimal',
      border = 'rounded',
      title = ' Help (hk) ',
      title_pos = 'center',
    })

    vim.keymap.set('n', '<Esc>', function()
      if vim.api.nvim_win_is_valid(win) then
        vim.api.nvim_win_close(win, true)
      end
      if vim.api.nvim_buf_is_valid(buf) then
        vim.api.nvim_buf_delete(buf, { force = true })
      end
    end, { buffer = buf, noremap = true, silent = true })
  end

  set("n", "<leader>hk", show_help, { desc = "Show key bindings help" })
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
          vim.api.nvim_set_option_value('winbar', '%=%#Title#[' .. tostring(i) .. ']%=',
            { win = win, scope = 'local' })
          vim.api.nvim_set_option_value('number', false, { win = win, scope = 'local' })
          vim.api.nvim_set_option_value('relativenumber', false,
            { win = win, scope = 'local' })
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
      set({ 'n', 'i', 'v' }, char, fn,
        vim.tbl_extend('force', opts, { desc = 'Focus/toggle window ' .. i }))
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
  local float_opts = { border = 'rounded', focusable = false }

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

      if false and vim.lsp.on_type_formatting and client:supports_method(vim.lsp.protocol.Methods.textDocument_onTypeFormatting, bufnr) then
        vim.lsp.on_type_formatting.enable(true, { client_id = client.id })
      end

      local group = vim.api.nvim_create_augroup("UserLsp_" .. bufnr, { clear = true })

      local clients = vim.lsp.get_clients({ bufnr = bufnr })
      local has_format, has_highlight, has_sig = false, false, false
      for _, c in ipairs(clients) do
        has_format = has_format or
            c:supports_method(vim.lsp.protocol.Methods.textDocument_formatting, bufnr)
        has_highlight = has_highlight or
            c:supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight, bufnr)
        has_sig = has_sig or
            c:supports_method(vim.lsp.protocol.Methods.textDocument_signatureHelp, bufnr)
      end

      if false and has_format then
        vim.api.nvim_create_autocmd('BufWritePre', {
          group = group,
          buffer = bufnr,
          callback = function() vim.lsp.buf.format({ bufnr = bufnr, timeout_ms = 1000 }) end,
        })
      end

      if has_highlight or has_sig then
        vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
          group = group,
          buffer = bufnr,
          callback = function()
            if has_highlight then vim.lsp.buf.document_highlight() end
            local mode = vim.api.nvim_get_mode().mode
            if has_sig and (mode == 'i' or mode == 'ic') then
              vim.lsp.buf.signature_help(float_opts)
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
      map('K', function() vim.lsp.buf.hover(float_opts) end, 'Hover')
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
  local mode_map = {
    n = 'NOR', no = 'O-P', nov = 'O-P', noV = 'O-P', ['no\22'] = 'O-P',
    niI = 'NOR', niR = 'NOR', niV = 'NOR', nt = 'NOR',
    v = 'VIS', vs = 'VIS', V = 'V-L', Vs = 'V-L',
    ['\22'] = 'V-B', ['\22s'] = 'V-B',
    s = 'SEL', S = 'S-L', ['\19'] = 'S-B',
    i = 'INS', ic = 'INS', ix = 'INS',
    R = 'REP', Rc = 'REP', Rx = 'REP', Rv = 'V-R', Rvc = 'V-R', Rvx = 'V-R',
    c = 'CMD', cv = 'EX', ce = 'EX',
    r = 'HIT', rm = 'MOR', ['r?'] = 'CON',
    ['!'] = 'SHL', t = 'TER',
  }

  function Statusline_mode()
    local mode = vim.api.nvim_get_mode().mode
    return mode_map[mode] or mode
  end

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
        progress_msg = table.concat(
          vim.tbl_filter(function(s) return s ~= '' end, { name, title, message, pct }), ' ')
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

local configure_autocmds = function(vim, root_markers)
  -- Every UI attach lands here: a launcher hands its UI over, a session reports
  -- the reattach. The first UIEnter is this instance's own startup.
  local own_ui_seen = false
  vim.api.nvim_create_autocmd('UIEnter', {
    callback = function()
      if joining then
        -- ! stops this launcher once the UI is gone instead of leaving it headless.
        vim.cmd('connect! ' .. vim.fn.fnameescape(socket_path))
      elseif own_ui_seen then
        local up = os.time() - math.floor(vim.v.starttime / 1e9)
        vim.schedule(function()
          local usage_hours = math.floor(up / 3600)
          local usage_minutes = math.floor((up % 3600) / 60)
          local usage_seconds = up % 60

          -- format it into a string
          local usage_text = string.format(
            "%s, %s, and %s",
            usage_hours == 1 and string.format("%d hour", usage_hours) or string.format("%d hours", usage_hours),
            usage_minutes == 1 and string.format("%d minute", usage_minutes)
              or string.format("%d minutes", usage_minutes),
            usage_seconds == 1 and string.format("%d second", usage_seconds)
              or string.format("%d seconds", usage_seconds)
          )

          vim.notify(('Reattached to session (up %s)'):format(usage_text), vim.log.levels.INFO)
        end)
      end
      own_ui_seen = true
    end,
  })

  -- Anything that would exit nvim (:q, :qa, :wq, ZZ, ZQ, ...) detaches the UI
  -- instead, keeping the server alive. ExitPre only fires when the quit really
  -- would exit, and nvim cancels that quit when the window it was about to close
  -- no longer exists -- hence the window swap. :restart/ZR are left alone; use
  -- :cquit to terminate the server.
  vim.api.nvim_create_autocmd('ExitPre', {
    callback = function()
      if vim.v.exitreason ~= 'quit' then return end
      if #vim.api.nvim_list_uis() == 0 then return end

      local win = vim.api.nvim_get_current_win()
      local view = vim.fn.winsaveview()
      if not pcall(vim.cmd, 'noautocmd split') then return end
      if not pcall(vim.api.nvim_win_close, win, false) then return end
      vim.fn.winrestview(view)

      local ok, err = pcall(vim.cmd, 'silent detach')
      if not ok then
        vim.notify('detach failed: ' .. tostring(err), vim.log.levels.WARN)
      end
    end,
  })

  -- A directory that should never become the CWD via automatic root detection.
  -- $HOME, any ancestor of $HOME (e.g. /Users), and the filesystem root (/ or C:/)
  -- are rejected so they only become CWD when opened explicitly.
  local function is_unsafe_auto_root(dir)
    if not dir or dir == '' then return true end
    dir = vim.fs.normalize(dir)
    -- A filesystem root (/ or C:/) is its own parent.
    if vim.fn.fnamemodify(dir, ':h') == dir then return true end
    local home = vim.uv.os_homedir()
    if home then
      home = vim.fs.normalize(home)
      if dir == home or vim.startswith(home .. '/', dir .. '/') then return true end
    end
    return false
  end

  -- If nvim was opened with a single file (no directory), cd to the file's
  -- directory, then walk up to find a project root and cd there if one exists.
  vim.api.nvim_create_autocmd('VimEnter', {
    once = true,
    callback = function()
      local args = vim.fn.argv()
      if #args ~= 1 then return end
      local arg = args[1]
      if vim.fn.isdirectory(arg) == 1 then return end
      local file = vim.fn.fnamemodify(arg, ':p')
      if file == '' or file:match('^%a[%w+.-]*://') then return end
      -- Resolve symlinks so a symlinked config dir (e.g. ~/.config/nvim ->
      -- ~/dotfiles/.config/nvim) finds the real project root instead of falling
      -- back to the launch directory.
      file = vim.fn.resolve(file)
      local file_dir = vim.fn.fnamemodify(file, ':h')
      if vim.fn.isdirectory(file_dir) ~= 1 then return end

      local git_root = vim.fs.root(file, root_markers)

      -- Prefer the nearest project root, but only when it is a safe target. If no
      -- marker is found all the way up to the filesystem root (or the only root
      -- found is unsafe, e.g. $HOME), fall back to the file's own directory.
      local target_dir = file_dir
      if git_root and not is_unsafe_auto_root(git_root) then
        target_dir = git_root
      end
      if vim.fn.isdirectory(target_dir) ~= 1 then return end
      if is_unsafe_auto_root(target_dir) then return end
      local ok, err = pcall(vim.cmd.cd, target_dir)
      if not ok then
        vim.notify('VimEnter cd failed: ' .. tostring(err), vim.log.levels.WARN)
      end
    end,
  })

  vim.api.nvim_create_autocmd('BufNew', {
    callback = function(ev)
      local new_buf = ev.buf
      if vim.bo[new_buf].buftype ~= '' then return end
      local name = vim.api.nvim_buf_get_name(new_buf)
      if name == '' then return end
      for _, b in ipairs(vim.api.nvim_list_bufs()) do
        if b ~= new_buf
            and vim.api.nvim_buf_is_loaded(b)
            and vim.api.nvim_buf_get_name(b) == name then
          vim.schedule(function()
            if not vim.api.nvim_buf_is_valid(new_buf) then return end
            local wins = vim.fn.win_findbuf(new_buf)
            for _, w in ipairs(wins) do
              if vim.api.nvim_win_is_valid(w) then
                vim.api.nvim_win_set_buf(w, b)
              end
            end
            pcall(vim.api.nvim_buf_delete, new_buf, { force = true })
          end)
          return
        end
      end
    end,
  })

  vim.api.nvim_create_autocmd('BufEnter', {
    callback = function(ev)
      if vim.bo[ev.buf].buftype ~= '' then return end
      local name = vim.api.nvim_buf_get_name(ev.buf)
      if name == '' then return end
      -- Resolve symlinks so a symlinked path finds the real project root.
      local root = vim.fs.root(vim.fn.resolve(name), root_markers)
      if not root then return end
      -- Never auto-lcd to $HOME, an ancestor of it, or the filesystem root.
      if is_unsafe_auto_root(root) then return end
      vim.cmd.lcd(root)
    end,
  })

  vim.api.nvim_create_autocmd('TermOpen', {
    pattern = 'term://*lazygit*',
    callback = function()
      local buf = vim.api.nvim_get_current_buf()
      vim.bo[buf].buflisted = false
      vim.cmd.startinsert()
    end,
  })

  vim.api.nvim_create_autocmd('PackChanged', {
    callback = function(ev)
      if ev.data.kind ~= 'install' and ev.data.kind ~= 'update' then return end

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
        local result = vim.system({ 'sh', '-c', build }, {
          cwd = ev.data.path,
          text = true,
        }):wait()
        if result.code ~= 0 then
          vim.notify(
            ('Build failed for %s (exit %d)\n%s'):format(name, result.code,
              result.stderr or ''),
            vim.log.levels.ERROR
          )
        end
      end
    end,
  })
end

local configure_user_commands = function(vim, root_markers)
  local function require_executables(...)
    for _, exe in ipairs({ ... }) do
      if vim.fn.executable(exe) == 0 then
        vim.notify(exe .. ' is not installed', vim.log.levels.ERROR)
        return false
      end
    end
    return true
  end

  local function open_file_or_switch(file, lnum, col)
    local existing = vim.fn.bufnr(file)
    if existing ~= -1 then
      vim.cmd('buffer ' .. existing)
    else
      vim.cmd('edit ' .. vim.fn.fnameescape(file))
    end
    if lnum then
      pcall(vim.api.nvim_win_set_cursor, 0, { lnum, (col or 1) - 1 })
    end
  end

  local function read_selected_line(path)
    if vim.fn.filereadable(path) ~= 1 then return '' end
    local lines = vim.fn.readfile(path)
    return lines[1] and vim.trim(lines[1]) or ''
  end

  -- Search root for the fzf pickers, derived from the current buffer rather than
  -- the ambient window cwd (which can still be $HOME when a file is opened from an
  -- oil buffer, a new tab, or a no-argument launch). Resolves symlinks, then
  -- prefers the nearest project marker, else the file's own directory.
  local function search_root()
    local name = vim.api.nvim_buf_get_name(0)
    if name:match('^oil://') then
      local ok, oil = pcall(require, 'oil')
      if ok and oil.get_current_dir then
        local dir = oil.get_current_dir()
        if dir and dir ~= '' then
          dir = vim.fn.resolve((dir:gsub('/$', '')))
          return vim.fs.root(dir, root_markers) or dir
        end
      end
    end
    if name == '' or name:match('^%a[%w+.-]*://') then
      return vim.fn.getcwd()
    end
    local file = vim.fn.resolve(vim.fn.fnamemodify(name, ':p'))
    local dir = vim.fn.fnamemodify(file, ':h')
    if vim.fn.isdirectory(dir) ~= 1 then return vim.fn.getcwd() end
    return vim.fs.root(file, root_markers) or dir
  end

  -- Join a fzf-selected (possibly relative) path onto the root used to produce it,
  -- so it opens correctly regardless of the window's cwd.
  local function resolve_under(root, path)
    if path == '' or path:sub(1, 1) == '/' then return path end
    return vim.fs.joinpath(root, path)
  end

  local function fzf_run(shell_cmd, opts)
    opts = opts or {}
    local layout = opts.layout or 'float'
    local prev_win = vim.api.nvim_get_current_win()
    local prev_buf = vim.api.nvim_win_get_buf(prev_win)
    local fzf_buf = vim.api.nvim_create_buf(false, true)
    local fzf_win
    local preview_state

    if layout == 'split' then
      local pbuf = vim.api.nvim_create_buf(false, true)
      vim.bo[pbuf].bufhidden = 'wipe'
      vim.cmd('botright ' .. (opts.split_height or 15) .. 'split')
      fzf_win = vim.api.nvim_get_current_win()
      vim.api.nvim_win_set_buf(fzf_win, fzf_buf)
      vim.wo[fzf_win].winfixheight = true
      vim.wo[fzf_win].statusline = ' '
      vim.api.nvim_win_set_buf(prev_win, pbuf)
      vim.wo[prev_win].number = true
      vim.wo[prev_win].cursorline = true
      preview_state = { buf = pbuf, current_file = nil }

      if opts.preview_file then
        local last_line = ''
        local on_preview = opts.on_preview or function(line, pwin)
          local file, lnum = line:match('^(.+):(%d+):%d+:')
          if not file or not lnum then return end
          lnum = tonumber(lnum)
          if not vim.api.nvim_win_is_valid(pwin) then return end
          if preview_state.current_file ~= file then
            preview_state.current_file = file
            local abs = file
            if abs:sub(1, 1) ~= '/' and opts.cwd then abs = vim.fs.joinpath(opts.cwd, abs) end
            abs = vim.fn.fnamemodify(abs, ':p')
            if vim.fn.filereadable(abs) == 1 then
              local nbuf = vim.fn.bufnr(abs)
              if nbuf == -1 then
                nbuf = vim.api.nvim_create_buf(false, true)
                vim.bo[nbuf].bufhidden = 'wipe'
                local content = vim.fn.readfile(abs)
                vim.api.nvim_buf_set_lines(nbuf, 0, -1, false, content)
                local ft = vim.filetype.match({ filename = abs })
                if ft then vim.bo[nbuf].filetype = ft end
              end
              vim.api.nvim_win_set_buf(pwin, nbuf)
              if vim.api.nvim_buf_is_valid(preview_state.buf) and preview_state.buf ~= nbuf then
                pcall(vim.api.nvim_buf_delete, preview_state.buf,
                  { force = true })
              end
              preview_state.buf = nbuf
            end
          end
          pcall(vim.api.nvim_win_set_cursor, pwin, { lnum, 0 })
          pcall(vim.api.nvim_win_call, pwin, function() vim.cmd('normal! zz') end)
        end
        preview_state.timer = vim.uv.new_timer()
        preview_state.timer:start(100, 100, vim.schedule_wrap(function()
          if vim.fn.filereadable(opts.preview_file) ~= 1 then return end
          local cur_lines = vim.fn.readfile(opts.preview_file)
          local cur = cur_lines[1] or ''
          if cur == '' or cur == last_line then return end
          last_line = cur
          on_preview(cur, prev_win)
        end))
      end
    else
      local width = math.floor(vim.o.columns * (opts.width_pct or 0.8))
      local height = opts.height or math.floor(vim.o.lines * (opts.height_pct or 0.6))
      fzf_win = vim.api.nvim_open_win(fzf_buf, true, {
        relative = 'editor',
        width = width,
        height = height,
        row = math.floor((vim.o.lines - height) / 2),
        col = math.floor((vim.o.columns - width) / 2),
        style = 'minimal',
        border = 'rounded',
      })
    end

    local function cleanup()
      if preview_state and preview_state.timer then
        preview_state.timer:stop()
        preview_state.timer:close()
        preview_state.timer = nil
      end
      if vim.api.nvim_win_is_valid(fzf_win) then
        vim.api.nvim_win_close(fzf_win, true)
      end
      if vim.api.nvim_buf_is_valid(fzf_buf) then
        pcall(vim.api.nvim_buf_delete, fzf_buf, { force = true })
      end
    end

    local job_id = vim.fn.termopen(shell_cmd, {
      cwd = (opts.cwd and vim.fn.isdirectory(opts.cwd) == 1) and opts.cwd or nil,
      on_exit = function(_, exit_code, _)
        vim.schedule(function()
          cleanup()
          if vim.api.nvim_win_is_valid(prev_win) then
            vim.api.nvim_set_current_win(prev_win)
          end
          if opts.on_result then
            opts.on_result(exit_code)
          end
          if preview_state then
            if exit_code ~= 0 and vim.api.nvim_win_is_valid(prev_win) and vim.api.nvim_buf_is_valid(prev_buf) then
              vim.api.nvim_win_set_buf(prev_win, prev_buf)
            end
            if vim.api.nvim_buf_is_valid(preview_state.buf) then
              pcall(vim.api.nvim_buf_delete, preview_state.buf,
                { force = true })
            end
          end
          for _, f in ipairs(opts.tmp_files or {}) do
            vim.fn.delete(f)
          end
        end)
      end,
    })

    vim.api.nvim_create_autocmd('TermClose', {
      buffer = fzf_buf,
      once = true,
      callback = function()
        if vim.api.nvim_win_is_valid(fzf_win) then
          vim.api.nvim_win_close(fzf_win, true)
        end
      end,
    })

    vim.keymap.set({ 'n', 't' }, '<Esc>', function()
      cleanup()
      pcall(vim.fn.jobstop, job_id)
    end, { buffer = fzf_buf, nowait = true })

    vim.cmd('startinsert')
  end

  local function run_fzf_selection(shell_cmd, opts)
    opts = opts or {}
    local output_file = vim.fn.tempname()
    local tmp_files = { output_file }
    if opts.tmp_files then
      vim.list_extend(tmp_files, opts.tmp_files)
    end

    fzf_run(shell_cmd .. ' > ' .. vim.fn.shellescape(output_file), {
      layout = opts.layout,
      cwd = opts.cwd,
      split_height = opts.split_height,
      preview_file = opts.preview_file,
      on_preview = opts.on_preview,
      tmp_files = tmp_files,
      on_result = function(exit_code)
        local selected = ''
        if exit_code == 0 then
          selected = read_selected_line(output_file)
        end
        if opts.on_result then
          opts.on_result(exit_code, selected)
        end
      end,
    })
  end

  local function run_fzf_from_lines(lines, opts)
    opts = opts or {}
    local input_file = vim.fn.tempname()
    vim.fn.writefile(lines, input_file)

    local fzf_cmd = opts.fzf_cmd
    if not fzf_cmd then
      local prompt = opts.prompt or 'Select> '
      fzf_cmd = 'fzf --layout=reverse --prompt=' .. vim.fn.shellescape(prompt)
    end

    local tmp_files = { input_file }
    if opts.tmp_files then
      vim.list_extend(tmp_files, opts.tmp_files)
    end

    run_fzf_selection('cat ' .. vim.fn.shellescape(input_file) .. ' | ' .. fzf_cmd, {
      layout = opts.layout,
      split_height = opts.split_height,
      preview_file = opts.preview_file,
      on_preview = opts.on_preview,
      tmp_files = tmp_files,
      on_result = opts.on_result,
    })
  end

  local function fzf_files(query)
    if not require_executables('fd', 'fzf') then return end
    local root = search_root()
    local fd_cmd = "fd --type f --strip-cwd-prefix --hidden --follow --exclude .git"
    local fzf_cmd = "fzf --height=100% --layout=reverse --prompt='Files> '"
    if query and query ~= '' then
      fzf_cmd = fzf_cmd .. ' --query ' .. vim.fn.shellescape(query)
    end
    run_fzf_selection(fd_cmd .. ' | ' .. fzf_cmd, {
      cwd = root,
      on_result = function(exit_code, selected)
        if exit_code ~= 0 then return end
        if selected ~= '' then open_file_or_switch(resolve_under(root, selected)) end
      end,
    })
  end

  vim.api.nvim_create_user_command('FzfFiles', function(args)
    fzf_files(args.args)
  end, { nargs = '?', desc = 'Pick files with fd + fzf' })

  local function get_recent_files()
    local seen = {}
    local files = {}
    for _, file in ipairs(vim.v.oldfiles or {}) do
      local abs = vim.fn.fnamemodify(file, ':p')
      if abs ~= ''
          and not seen[abs]
          and vim.fn.filereadable(abs) == 1
          and not abs:match('^term://')
      then
        seen[abs] = true
        table.insert(files, abs)
      end
    end
    return files
  end

  local function fzf_recents()
    if not require_executables('fzf') then return end
    local recents = get_recent_files()
    if #recents == 0 then
      vim.notify('No recent files found', vim.log.levels.INFO)
      return
    end

    local entries = vim.tbl_map(function(path)
      return vim.fn.fnamemodify(path, ':~:.')
    end, recents)

    run_fzf_from_lines(entries, {
      prompt = 'Recents> ',
      on_result = function(exit_code, selected)
        if exit_code ~= 0 then return end
        if selected == '' then return end
        open_file_or_switch(vim.fn.fnamemodify(selected, ':p'))
      end,
    })
  end

  local function fzf_recent_dirs()
    if not require_executables('fzf') then return end
    local seen = {}
    local dirs = {}
    for _, file in ipairs(get_recent_files()) do
      local dir = vim.fn.fnamemodify(file, ':p:h')
      if dir ~= '' and not seen[dir] and vim.fn.isdirectory(dir) == 1 then
        seen[dir] = true
        table.insert(dirs, dir)
      end
    end

    if #dirs == 0 then
      vim.notify('No recent folders found', vim.log.levels.INFO)
      return
    end

    local entries = vim.tbl_map(function(path)
      return vim.fn.fnamemodify(path, ':~:.')
    end, dirs)

    run_fzf_from_lines(entries, {
      prompt = 'RecentDirs> ',
      on_result = function(exit_code, selected)
        if exit_code ~= 0 then return end
        if selected == '' then return end
        local dir = vim.fn.fnamemodify(selected, ':p')
        if vim.fn.isdirectory(dir) == 0 then return end
        vim.cmd('cd ' .. vim.fn.fnameescape(dir))
        local ok, oil = pcall(require, 'oil')
        if ok and oil and oil.open then
          oil.open(dir)
        else
          vim.cmd('edit ' .. vim.fn.fnameescape(dir))
        end
      end,
    })
  end

  vim.api.nvim_create_user_command('FzfRecents', function()
    fzf_recents()
  end, { desc = 'Pick recent file with fzf' })

  vim.api.nvim_create_user_command('FzfRecentDirs', function()
    fzf_recent_dirs()
  end, { desc = 'Pick recent folder with fzf' })

  local function fzf_grep(query, dir)
    if not require_executables('rg', 'fzf') then return end
    local cwd = dir or search_root()
    local tmp = vim.fn.tempname()
    local initial_query = (query and query ~= '') and query or ''
    local path_arg = dir and (' ' .. vim.fn.shellescape(dir)) or ''
    local rg_base = 'rg --column --line-number --no-heading --color=always --smart-case'
    local fzf_cmd = table.concat({
      'fzf', '--ansi', '--disabled', '--layout=reverse', '--delimiter=:',
      '--bind', vim.fn.shellescape('change:reload:' .. rg_base .. ' -- {q}' .. path_arg .. ' || true'),
      '--query', vim.fn.shellescape(initial_query),
      "--prompt='Grep> '",
    }, ' ')
    local shell_cmd = table.concat({
      rg_base .. ' --',
      vim.fn.shellescape(initial_query ~= '' and initial_query or '.'),
      path_arg, '|', fzf_cmd, '>', vim.fn.shellescape(tmp),
    }, ' ')
    fzf_run(shell_cmd, {
      cwd = cwd,
      tmp_files = { tmp },
      on_result = function(exit_code)
        if exit_code ~= 0 then return end
        local lines = vim.fn.filereadable(tmp) == 1 and vim.fn.readfile(tmp) or {}
        local selected = lines[1] and vim.trim(lines[1]) or ''
        if selected == '' then return end
        local file, lnum, col = selected:match('^(.+):(%d+):(%d+):')
        if file then open_file_or_switch(resolve_under(cwd, file), tonumber(lnum), tonumber(col)) end
      end,
    })
  end

  vim.api.nvim_create_user_command('FzfGrep', function(args)
    fzf_grep(args.args)
  end, { nargs = '?', desc = 'Live grep with rg + fzf' })

  vim.api.nvim_create_user_command('FzfGrepDir', function(args)
    local dir = vim.fn.expand('%:p:h')
    fzf_grep(args.args, dir)
  end, { nargs = '?', desc = 'Live grep in current file directory with rg + fzf' })

  local function fzf_live_grep(query)
    if not require_executables('rg', 'fzf') then return end
    local root = search_root()
    local tmp = vim.fn.tempname()
    local cur_file = tmp .. '.cur'
    local initial_query = (query and query ~= '') and query or ''
    local rg_base = 'rg --column --line-number --no-heading --color=always --smart-case'
    local fzf_cmd = table.concat({
      'fzf', '--ansi', '--disabled', '--layout=reverse', '--delimiter=:',
      '--bind', vim.fn.shellescape('change:reload:' .. rg_base .. ' -- {q} || true'),
      '--bind', vim.fn.shellescape('focus:execute-silent(echo {+} > ' .. cur_file .. ')'),
      '--query', vim.fn.shellescape(initial_query),
      "--prompt='LiveGrep> '",
    }, ' ')
    local shell_cmd = table.concat({
      rg_base .. ' --',
      vim.fn.shellescape(initial_query ~= '' and initial_query or '.'),
      '|', fzf_cmd, '>', vim.fn.shellescape(tmp),
    }, ' ')
    fzf_run(shell_cmd, {
      layout = 'split',
      cwd = root,
      preview_file = cur_file,
      tmp_files = { tmp, cur_file },
      on_result = function(exit_code)
        if exit_code ~= 0 then return end
        local lines = vim.fn.filereadable(tmp) == 1 and vim.fn.readfile(tmp) or {}
        local selected = lines[1] and vim.trim(lines[1]) or ''
        if selected == '' then return end
        local file, lnum, col = selected:match('^(.+):(%d+):(%d+):')
        if file then open_file_or_switch(resolve_under(root, file), tonumber(lnum), tonumber(col)) end
      end,
    })
  end

  vim.api.nvim_create_user_command('FzfLiveGrep', function(args)
    fzf_live_grep(args.args)
  end, { nargs = '?', desc = 'Live grep with split preview' })

  _G._term_last_output = _G._term_last_output or {}

  local function term_has_fg_child(pid)
    if vim.fn.has('win32') == 1 then
      local r = vim.system({ 'powershell', '-NoProfile', '-Command',
        string.format('(Get-CimInstance Win32_Process -Filter "ParentProcessId=%d").Count', pid)
      }):wait()
      local count = tonumber(vim.trim(r.stdout or ''))
      return count and count > 0
    else
      local r = vim.system({ 'ps', '-o', 'pgid=,tpgid=', '-p', tostring(pid) }):wait()
      if r.code ~= 0 then return false end
      local pgid, tpgid = vim.trim(r.stdout or ''):match('(%d+)%s+(%d+)')
      if not pgid or not tpgid then return false end
      return pgid ~= tpgid
    end
  end

  local function term_is_idle(bufnr)
    local chan = vim.bo[bufnr].channel
    if chan == 0 then return false end
    local last = _G._term_last_output[bufnr]
    if last and (vim.uv.now() - last) <= 1000 then return false end
    local ok, pid = pcall(vim.fn.jobpid, chan)
    if not ok or not pid then return true end
    return not term_has_fg_child(pid)
  end

  local function create_terminal()
    vim.cmd('enew')
    vim.fn.termopen(vim.o.shell)
    vim.cmd('startinsert')
  end

  local function create_terminal_command(command)
    vim.cmd('enew')
    vim.fn.termopen(command)
    vim.cmd('startinsert')
  end

  local function fzf_buf_picker(opts)
    if not require_executables('fzf') then return end
    opts = opts or {}
    local filter = opts.filter or function(b)
      return vim.api.nvim_buf_is_loaded(b) and vim.bo[b].buflisted
    end
    local prompt = opts.prompt or 'Buffers> '
    local format_entry = opts.format_entry or function(b)
      local raw = vim.api.nvim_buf_get_name(b)
      local name = raw ~= '' and vim.fn.fnamemodify(raw, ':~:.') or ('[buf ' .. b .. ']')
      local modified = vim.bo[b].modified and ' [+]' or ''
      local bell = (_G._term_bell_bufs and _G._term_bell_bufs[b]) and ' [bell]' or ''
      return string.format('%d: %s%s%s', b, name, modified, bell)
    end

    local bufs = vim.tbl_filter(filter, vim.api.nvim_list_bufs())

    if #bufs == 0 then
      vim.notify('No matching buffers', vim.log.levels.INFO)
      return
    end

    local cur_file = vim.fn.tempname()
    local lines = {}
    local current_buf = vim.api.nvim_get_current_buf()
    local current_line = nil
    for _, b in ipairs(bufs) do
      local entry = format_entry(b)
      if b == current_buf then
        current_line = entry
      else
        table.insert(lines, entry)
      end
    end
    if current_line then table.insert(lines, 1, current_line) end
    local fzf_cmd = table.concat({
      'fzf', '--layout=reverse',
      '--bind', vim.fn.shellescape('focus:execute-silent(echo {} > ' .. cur_file .. ')'),
      '--prompt', vim.fn.shellescape(prompt),
    }, ' ')

    run_fzf_from_lines(lines, {
      fzf_cmd = fzf_cmd,
      layout = 'split',
      split_height = math.min(#bufs + 4, 15),
      preview_file = cur_file,
      on_preview = function(line, pwin)
        local bnr = line:match('^(%d+):')
        if not bnr then return end
        bnr = tonumber(bnr)
        if not vim.api.nvim_win_is_valid(pwin) then return end
        if vim.api.nvim_buf_is_valid(bnr) then
          vim.api.nvim_win_set_buf(pwin, bnr)
        end
      end,
      tmp_files = { cur_file },
      on_result = function(exit_code, selected)
        if exit_code ~= 0 then return end
        local bufnr = selected:match('^(%d+):')
        if bufnr and vim.api.nvim_buf_is_valid(tonumber(bufnr)) then
          vim.cmd('buffer ' .. bufnr)
        end
      end,
    })
  end

  vim.api.nvim_create_user_command('FzfBuffers', function()
    fzf_buf_picker()
  end, { desc = 'Pick buffer with fzf' })

  vim.api.nvim_create_user_command('FzfTerminals', function()
    fzf_buf_picker({
      prompt = 'Terminals> ',
      filter = function(b)
        return vim.api.nvim_buf_is_valid(b)
            and vim.bo[b].buftype == 'terminal'
            and vim.bo[b].channel ~= 0
      end,
      format_entry = function(b)
        local raw = vim.api.nvim_buf_get_name(b)
        local name = raw:match('term://(.+)') or raw
        if name == '' then name = '[terminal ' .. b .. ']' end
        local idle = term_is_idle(b) and '' or ' [busy]'
        local bell = (_G._term_bell_bufs and _G._term_bell_bufs[b]) and ' [bell]' or ''
        return string.format('%d: %s%s%s', b, name, idle, bell)
      end,
    })
  end, { desc = 'Pick terminal with fzf' })

  vim.api.nvim_create_user_command('FzfAgentTerminals', function()
    fzf_buf_picker({
      prompt = 'AgentTerms> ',
      filter = function(b)
        return vim.api.nvim_buf_is_valid(b)
            and vim.bo[b].buftype == 'terminal'
            and vim.bo[b].channel ~= 0
            and vim.api.nvim_buf_get_name(b):match('agent') ~= nil
      end,
      format_entry = function(b)
        local raw = vim.api.nvim_buf_get_name(b)
        local name = raw:match('term://(.+)') or raw
        if name == '' then name = '[terminal ' .. b .. ']' end
        local idle = term_is_idle(b) and '' or ' [busy]'
        local bell = (_G._term_bell_bufs and _G._term_bell_bufs[b]) and ' [bell]' or ''
        return string.format('%d: %s%s%s', b, name, idle, bell)
      end,
    })
  end, { desc = 'Pick Agent terminal with fzf' })

  vim.api.nvim_create_user_command('TermNew', function()
    create_terminal()
  end, { desc = 'Create a new terminal buffer' })

  vim.api.nvim_create_user_command('AgentTermNew', function()
    if not require_executables('omp') then return end
    create_terminal_command('omp')
  end, { desc = 'Create a new agent terminal buffer' })

  vim.api.nvim_create_user_command('AgentTermNewWithFlags', function()
    vim.ui.input({
      prompt = 'Agent command: ',
      default = 'omp',
    }, function(input)
      if input == nil then return end
      local command = vim.trim(input)
      if command == '' then command = 'omp' end
      local exe = command:match('^%S+')
      if not require_executables(exe) then return end
      create_terminal_command(command)
    end)
  end, { desc = 'Create a new agent terminal buffer with a custom command' })

  vim.api.nvim_create_user_command('TermNext', function()
    local current = vim.api.nvim_get_current_buf()
    local term_bufs = vim.tbl_filter(function(b)
      return vim.api.nvim_buf_is_valid(b)
          and vim.bo[b].buftype == 'terminal'
          and vim.bo[b].channel ~= 0
    end, vim.api.nvim_list_bufs())

    local idle = {}
    for _, b in ipairs(term_bufs) do
      if term_is_idle(b) then
        table.insert(idle, b)
      end
    end

    if #idle == 0 then
      create_terminal()
      return
    end

    local target = idle[1]
    for _, b in ipairs(idle) do
      if b > current then
        target = b
        break
      end
    end

    vim.cmd('buffer ' .. target)
    vim.cmd('startinsert')
  end, { desc = 'Switch to next idle terminal or create new' })

  vim.api.nvim_create_user_command('LazyGit', function()
    vim.cmd('terminal lazygit')
  end, { desc = 'Open LazyGit' })

  vim.api.nvim_create_user_command('LazyGitCurrentFile', function()
    local file = vim.fn.expand('%:p')
    vim.cmd('terminal lazygit --filter ' .. vim.fn.shellescape(file))
  end, { desc = 'Open LazyGit filtered on current file' })

  vim.api.nvim_create_user_command('LazyGitFilter', function(args)
    vim.cmd('terminal lazygit --filter ' .. vim.fn.shellescape(args.args))
  end, { nargs = 1, desc = 'Open LazyGit with filter' })

  vim.api.nvim_create_user_command('KillServer', function()
    vim.fn.serverstop(socket_path)
    vim.cmd('qa!')
  end, { desc = 'Force kill the background Neovim server completely' })

end

local configure_term_bell_indicator = function()
  local term_bell_overlay = { win = nil, buf = nil }
  _G._term_bell_bufs = _G._term_bell_bufs or {}

  local function term_bell_count()
    local n = 0
    for b, _ in pairs(_G._term_bell_bufs) do
      if vim.api.nvim_buf_is_valid(b) then
        n = n + 1
      else
        _G._term_bell_bufs[b] = nil
      end
    end
    return n
  end

  local function ensure_overlay_buf()
    if term_bell_overlay.buf and vim.api.nvim_buf_is_valid(term_bell_overlay.buf) then return end
    term_bell_overlay.buf = vim.api.nvim_create_buf(false, true)
    vim.bo[term_bell_overlay.buf].buftype = 'nofile'
    vim.bo[term_bell_overlay.buf].bufhidden = 'hide'
    vim.bo[term_bell_overlay.buf].buflisted = false
  end

  local function refresh_term_bell_overlay()
    local n = term_bell_count()
    if n == 0 then
      if term_bell_overlay.win and vim.api.nvim_win_is_valid(term_bell_overlay.win) then
        vim.api.nvim_win_close(term_bell_overlay.win, true)
        term_bell_overlay.win = nil
      end
      return
    end
    ensure_overlay_buf()
    local dots = table.concat(vim.fn['repeat']({ '●' }, n), ' ')
    vim.api.nvim_buf_set_lines(term_bell_overlay.buf, 0, -1, false, { ' ' .. dots .. ' ' })
    local width = n * 2 + 1
    if term_bell_overlay.win and vim.api.nvim_win_is_valid(term_bell_overlay.win) then
      vim.api.nvim_win_set_config(term_bell_overlay.win, {
        relative = 'editor',
        width = width,
        row = 0,
        col = vim.o.columns - width - 2,
      })
      return
    end
    term_bell_overlay.win = vim.api.nvim_open_win(term_bell_overlay.buf, false, {
      relative = 'editor',
      width = width,
      height = 1,
      row = 0,
      col = vim.o.columns - width - 2,
      style = 'minimal',
      border = 'rounded',
      focusable = false,
      zindex = 100,
    })
  end

  vim.fn.termopen = function(cmd, opts)
    opts = opts or {}
    opts.term = true
    local user_stdout = opts.on_stdout
    opts.on_stdout = function(job_id, data, event)
      local info = vim.api.nvim_get_chan_info(job_id)
      local term_buf = info and info.buffer or 0
      if term_buf > 0 then
        _G._term_last_output[term_buf] = vim.uv.now()
      end
      for _, chunk in ipairs(data) do
        if chunk:find('\a') or chunk:find('\x07') then
          if term_buf > 0 then
            vim.schedule(function()
              _G._term_bell_bufs[term_buf] = true
              refresh_term_bell_overlay()
            end)
          end
          break
        end
      end
      if user_stdout then user_stdout(job_id, data, event) end
    end
    return vim.fn.jobstart(cmd, opts)
  end

  vim.api.nvim_create_autocmd('BufEnter', {
    callback = function(ev)
      if not _G._term_bell_bufs[ev.buf] then return end
      local win = vim.api.nvim_get_current_win()
      local config = vim.api.nvim_win_get_config(win)
      if config.relative ~= '' then return end
      _G._term_bell_bufs[ev.buf] = nil
      refresh_term_bell_overlay()
    end,
  })

  vim.api.nvim_create_autocmd('BufDelete', {
    callback = function(ev)
      if _G._term_bell_bufs[ev.buf] then
        _G._term_bell_bufs[ev.buf] = nil
        vim.schedule(refresh_term_bell_overlay)
      end
    end,
  })

  vim.api.nvim_create_autocmd('VimResized', {
    callback = function() refresh_term_bell_overlay() end,
  })

  vim.api.nvim_create_user_command('TermBellDismiss', function()
    _G._term_bell_bufs = {}
    refresh_term_bell_overlay()
  end, { desc = 'Dismiss all terminal bell indicators' })
  vim.api.nvim_create_user_command('TermBellTest', function()
    _G._term_bell_bufs[-1] = true
    refresh_term_bell_overlay()
  end, { desc = 'Test terminal bell indicator' })
end
local configure_centered_view = function(vim)
  -- Content column = ratio * width, clamped to [min, max] for readability on
  -- very wide (ultrawide) and narrow windows.
  local ratio, min_content, max_content = 0.62, 72, 120
  local max_gutter = 46

  local active = {}
  local busy = false

  local function sign_cols(winid)
    local sc = vim.api.nvim_get_option_value('signcolumn', { win = winid })
    if sc == 'no' then return 0 end
    -- 'yes'/'auto' -> 1 group (2 cols); 'yes:N'/'auto:N' -> N groups.
    local n = tonumber(sc:match(':(%d)')) or 1
    return 2 * n
  end

  local function compute_pad(total, buf, winid)
    local content = math.max(min_content, math.min(max_content, math.floor(total * ratio)))
    local nwidth = math.max(3, #tostring(vim.api.nvim_buf_line_count(buf)))
    local gutter = sign_cols(winid) + nwidth + 1
    local pad = math.floor((total - gutter - content) / 2)
    return math.min(pad, max_gutter - gutter)
  end

  function _G.__centered_view_pad()
    local winid = vim.g.statusline_winid
    if winid == nil or winid == 0 then winid = vim.api.nvim_get_current_win() end
    if not vim.api.nvim_win_is_valid(winid) then return '' end
    local pad = vim.w[winid].centered_view_pad
    if not pad or pad < 1 then return '' end
    return string.rep(' ', pad)
  end

  local number_seg = [[%{(&number || &relativenumber) ? (v:virtnum == 0 ? printf('%'.max([3,strlen(line('$'))]).'d ', &relativenumber ? (v:relnum==0 ? v:lnum : v:relnum) : v:lnum) : repeat(' ', max([3,strlen(line('$'))]) + 1)) : ''}]]
  local centered_stc = "%{v:lua.__centered_view_pad()}%s" .. number_seg

  local function make_pad_win(width)
    vim.cmd('noautocmd rightbelow vnew')
    local win = vim.api.nvim_get_current_win()
    local buf = vim.api.nvim_get_current_buf()
    vim.api.nvim_win_set_width(win, width)
    for _, o in ipairs({ 'number', 'relativenumber', 'cursorline', 'cursorcolumn', 'list', 'spell', 'wrap' }) do
      vim.api.nvim_set_option_value(o, false, { win = win })
    end
    vim.api.nvim_set_option_value('signcolumn', 'no', { win = win })
    vim.api.nvim_set_option_value('foldcolumn', '0', { win = win })
    vim.api.nvim_set_option_value('statuscolumn', '', { win = win })
    vim.api.nvim_set_option_value('winfixwidth', true, { win = win })
    vim.api.nvim_set_option_value('fillchars', 'eob: ', { win = win })
    vim.api.nvim_set_option_value('winhighlight', 'Normal:Normal,EndOfBuffer:Normal', { win = win })
    vim.api.nvim_set_option_value('buftype', 'nofile', { buf = buf })
    vim.api.nvim_set_option_value('bufhidden', 'wipe', { buf = buf })
    vim.api.nvim_set_option_value('swapfile', false, { buf = buf })
    vim.api.nvim_set_option_value('buflisted', false, { buf = buf })
    vim.api.nvim_set_option_value('modifiable', false, { buf = buf })
    return win
  end

  local function disable(content)
    local info = active[content]
    active[content] = nil
    busy = true
    if info and info.right and vim.api.nvim_win_is_valid(info.right) then
      pcall(vim.api.nvim_win_close, info.right, true)
    end
    if vim.api.nvim_win_is_valid(content) then
      vim.w[content].centered_view = nil
      vim.w[content].centered_view_pad = nil
      vim.api.nvim_set_option_value('statuscolumn', '', { win = content })
      if info then
        vim.api.nvim_set_option_value('wrap', info.wrap, { win = content })
        vim.api.nvim_set_option_value('fillchars', info.fcs, { win = content })
      end
    end
    busy = false
    if vim.api.nvim_win_is_valid(content) then vim.cmd('redraw') end
  end

  local function enable(content)
    if vim.api.nvim_win_get_config(content).relative ~= '' then
      vim.notify('Centered view: not available in floating windows', vim.log.levels.WARN)
      return
    end
    local buf = vim.api.nvim_win_get_buf(content)
    if vim.api.nvim_get_option_value('buftype', { buf = buf }) ~= '' then
      vim.notify('Centered view: only available for normal buffers', vim.log.levels.WARN)
      return
    end
    local total = vim.api.nvim_win_get_width(content)
    local pad = compute_pad(total, buf, content)
    if pad < 1 then
      vim.notify('Centered view: window too narrow', vim.log.levels.WARN)
      return
    end
    local saved_wrap = vim.api.nvim_get_option_value('wrap', { win = content })
    local saved_fcs = vim.api.nvim_get_option_value('fillchars', { win = content })
    busy = true
    local right = make_pad_win(pad)
    vim.api.nvim_set_current_win(content)
    busy = false
    active[content] = { right = right, wrap = saved_wrap, fcs = saved_fcs }
    vim.w[content].centered_view = true
    vim.w[content].centered_view_pad = pad
    vim.api.nvim_set_option_value('wrap', true, { win = content })
    vim.api.nvim_set_option_value('statuscolumn', centered_stc, { win = content })
    vim.api.nvim_set_option_value('fillchars', 'eob: ,vert: ', { win = content })
    vim.cmd('redraw')
  end

  local function toggle(content)
    content = content or vim.api.nvim_get_current_win()
    if active[content] or vim.w[content].centered_view then
      disable(content)
    else
      enable(content)
    end
  end

  local group = vim.api.nvim_create_augroup('CenteredView', { clear = true })

  vim.api.nvim_create_autocmd('VimResized', {
    group = group,
    callback = function()
      if busy then return end
      for content, info in pairs(active) do
        if vim.api.nvim_win_is_valid(content) and info.right and vim.api.nvim_win_is_valid(info.right) then
          local total = vim.api.nvim_win_get_width(content) + vim.api.nvim_win_get_width(info.right) + 1
          local pad = compute_pad(total, vim.api.nvim_win_get_buf(content), content)
          if pad < 1 then
            disable(content)
          else
            vim.w[content].centered_view_pad = pad
            busy = true
            pcall(vim.api.nvim_win_set_width, info.right, pad)
            busy = false
          end
        else
          disable(content)
        end
      end
      vim.cmd('redraw')
    end,
  })

  vim.api.nvim_create_autocmd('WinClosed', {
    group = group,
    callback = function(args)
      if busy then return end
      local closed = tonumber(args.match)
      if not closed then return end
      local info = active[closed]
      if info then
        active[closed] = nil
        if info.right and vim.api.nvim_win_is_valid(info.right) then
          busy = true
          pcall(vim.api.nvim_win_close, info.right, true)
          busy = false
        end
        return
      end
      for content, i in pairs(active) do
        if i.right == closed then
          disable(content)
          return
        end
      end
    end,
  })

  vim.api.nvim_create_user_command('CenteredViewToggle', function()
    toggle()
  end, { desc = 'Toggle centered (VSCode-style) view in the current window' })

  vim.keymap.set('n', '<leader>cv', function() toggle() end,
    { noremap = true, silent = true, desc = 'Toggle centered view' })
end

configure_defaults(vim)
configure_global_keymaps(vim)
configure_autocmds(vim, project_root_markers)
configure_user_commands(vim, project_root_markers)
configure_window_management()
configure_term_bell_indicator()
configure_centered_view(vim)
configure_lsp(vim, lsp_configs)
