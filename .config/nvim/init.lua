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

---@diagnostic disable-next-line: redefined-local
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
  use 'nvim-lua/plenary.nvim'

  -- UI, theme & related:
  use 'folke/tokyonight.nvim'
  use 'kyazdani42/nvim-web-devicons'
  use {"shortcuts/no-neck-pain.nvim"}
  use { 'hoob3rt/lualine.nvim', requires = { 'kyazdani42/nvim-web-devicons', opt = false } }
  use { 'luukvbaal/statuscol.nvim' }
  use { 'nvim-telescope/telescope.nvim', requires = 'nvim-lua/plenary.nvim' }
  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build', requires = { 'nvim-telescope/telescope.nvim' } }
  use {'nvim-telescope/telescope-ui-select.nvim', requires = { 'nvim-telescope/telescope.nvim' } }
  use {'debugloop/telescope-undo.nvim', requires = { 'nvim-telescope/telescope.nvim' } }

  -- Language specifics, including LSP, DAP, CMP, treesitter etc.
  use {
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',
      'WhoIsSethDaniel/mason-tool-installer.nvim',
      'neovim/nvim-lspconfig'
  }

  use { "windwp/nvim-autopairs" }

  use {
      'mfussenegger/nvim-dap',
      'jayp0521/mason-nvim-dap.nvim',
      'rcarriga/nvim-dap-ui',
      'theHamsta/nvim-dap-virtual-text'
  }

  use({
    "nvim-neotest/neotest",
    requires = {
      {
        "Issafalcon/neotest-dotnet",
      },
    }
  })

  use {
    'zbirenbaum/copilot.lua',
  }

  use {
      'hrsh7th/nvim-cmp',
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-nvim-lsp-document-symbol',
      'hrsh7th/cmp-nvim-lsp-signature-help',
      'hrsh7th/cmp-calc',
      'petertriho/cmp-git',
      'saadparwaiz1/cmp_luasnip',
  }

  use {
    "zbirenbaum/copilot-cmp",
  }

  use { "onsails/lspkind.nvim" }
  use 'https://git.sr.ht/~whynothugo/lsp_lines.nvim'
  use 'lvimuser/lsp-inlayhints.nvim'
  use { 'weilbith/nvim-code-action-menu' }

  use { 'dnlhc/glance.nvim' }

  use { 'L3MON4D3/LuaSnip', requires = "rafamadriz/friendly-snippets" }
  use { 'jose-elias-alvarez/null-ls.nvim', requires = "nvim-lua/plenary.nvim" }
  use { "utilyre/barbecue.nvim",
        branch = "main",
        requires = {
          "neovim/nvim-lspconfig",
          "smiteshp/nvim-navic",
          "kyazdani42/nvim-web-devicons",
        }
  }
  use 'j-hui/fidget.nvim'
  use {
    'nvim-treesitter/nvim-treesitter',
    requires = {
      'nvim-treesitter/playground'
    },
    config = 'vim.cmd [[TSUpdate]]'
  }
  use 'nvim-treesitter/nvim-treesitter-context'

  use 'RRethy/vim-illuminate'

  use { 'michaelb/sniprun', run = 'bash ./install.sh 1'}

  use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } }
  use { 'pwntester/octo.nvim',
    requires = {
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope.nvim',
      'kyazdani42/nvim-web-devicons',
    },
  }
  use { 'TimUntersberger/neogit', requires = 'nvim-lua/plenary.nvim' }
  use { 'sindrets/diffview.nvim', requires = 'nvim-lua/plenary.nvim' }
  use { 'antoinemadec/FixCursorHold.nvim' }
  use { 'stevearc/aerial.nvim' }
  use { 'saecki/crates.nvim', requires = { 'nvim-lua/plenary.nvim' } }
  use 'adelarsq/neofsharp.vim'
  use {'kevinhwang91/nvim-ufo', requires = 'kevinhwang91/promise-async'}
  use 'haringsrob/nvim_context_vt'

  use { 'stevearc/oil.nvim' }

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

-- All of floating windoww for diagnostics are from lspsaga.
local virt_ns = vim.api.nvim_create_namespace('FloatingDiagnostic')
local diag_augroup = vim.api.nvim_create_augroup('FloatingDiagnostic', { clear = true })


---@diagnostic disable-next-line: lowercase-global
function get_max_float_width()
  -- current window width
  local WIN_WIDTH = vim.fn.winwidth(0)
  local max_width = math.floor(WIN_WIDTH * 0.7)
  return max_width
end

local function get_border_style(style, highlight)
  highlight = highlight or 'FloatBorder'
  local border_style = {
    ['none'] = 'none',
    ['single'] = 'single',
    ['double'] = 'double',
    ['rounded'] = 'rounded',
    ['bold'] = {
      { '┏', highlight },
      { '─', highlight },
      { '┓', highlight },
      { '│', highlight },
      { '┛', highlight },
      { '─', highlight },
      { '┗', highlight },
      { '│', highlight },
    },
    ['plus'] = {
      { '+', highlight },
      { '─', highlight },
      { '+', highlight },
      { '│', highlight },
      { '+', highlight },
      { '─', highlight },
      { '+', highlight },
      { '│', highlight },
    },
  }

  return border_style[style]
end

local function make_floating_popup_options(width, height, opts)
  local api = vim.api
  vim.validate({
    opts = { opts, 't', true },
  })
  opts = opts or {}
  vim.validate({
    ['opts.offset_x'] = { opts.offset_x, 'n', true },
    ['opts.offset_y'] = { opts.offset_y, 'n', true },
  })
  local new_option = {}

  new_option.style = 'minimal'
  new_option.width = width
  new_option.height = height
  new_option.focusable = true
  if opts.focusable then
    new_option.focusable = opts.focusable
  end

  if opts.noautocmd then
    new_option.noautocmd = opts.noautocmd
  end

  if opts.relative ~= nil then
    new_option.relative = opts.relative
  else
    new_option.relative = 'cursor'
  end

  if opts.anchor ~= nil then
    new_option.anchor = opts.anchor
  end

  if opts.row == nil and opts.col == nil then
    local lines_above = vim.fn.winline() - 1
    local lines_below = vim.fn.winheight(0) - lines_above
    new_option.anchor = ''

    local pum_pos = vim.fn.pum_getpos()
    local pum_vis = not vim.tbl_isempty(pum_pos) -- pumvisible() can be true and pum_pos() returns {}
    if pum_vis and vim.fn.line('.') >= pum_pos.row or not pum_vis and lines_above < lines_below then
      new_option.anchor = 'N'
      new_option.row = 1
    else
      new_option.anchor = 'S'
      new_option.row = 0
    end

    if vim.fn.wincol() + width <= api.nvim_get_option('columns') then
      new_option.anchor = new_option.anchor .. 'W'
      new_option.col = 0
      if opts.move_col then
        new_option.col = new_option.col + opts.move_col
      end
    else
      new_option.anchor = new_option.anchor .. 'E'
      new_option.col = 1
      if opts.move_col then
        new_option.col = new_option.col - opts.move_col + 1
      end
    end
  else
    new_option.row = opts.row
    new_option.col = opts.col
  end

  return new_option
end

local function generate_win_opts(contents, opts)
  opts = opts or {}
  local win_width, win_height
  -- _make_floating_popup_size doesn't allow the window size to be larger than
  -- the current window. For the finder preview window, this means it won't let the
  -- preview window be wider than the finder window. To work around this, the
  -- no_size_override option can be set to indicate that the size shouldn't be changed
  -- from what was given.
  if opts.no_size_override and opts.width and opts.height then
    win_width, win_height = opts.width, opts.height
  else
    win_width, win_height = vim.lsp.util._make_floating_popup_size(contents, opts)
  end

  opts = make_floating_popup_options(win_width, win_height, opts)
  return opts
end

function create_win_with_border(content_opts, opts)
  local api = vim.api
  vim.validate({
    content_opts = { content_opts, 't' },
    contents = { content_opts.content, 't', true },
    opts = { opts, 't', true },
  })

  local contents, filetype = content_opts.contents, content_opts.filetype
  local enter = content_opts.enter or false
  local highlight = content_opts.highlight or 'LspFloatWinBorder'
  opts = opts or {}
  opts = generate_win_opts(contents, opts)
  -- TODO: Make configurable?
  opts.border = content_opts.border or get_border_style('rounded', highlight)

  -- create contents buffer
  local bufnr = content_opts.bufnr or api.nvim_create_buf(false, true)
  -- buffer settings for contents buffer
  -- Clean up input: trim empty lines from the end, pad
  local content = vim.lsp.util._trim(contents)

  if filetype then
    api.nvim_buf_set_option(bufnr, 'filetype', filetype)
  end

  content = vim.tbl_flatten(vim.tbl_map(function(line)
    if string.find(line, '\n') then
      return vim.split(line, '\n')
    end
    return line
  end, content))

  if not vim.tbl_isempty(content) then
    api.nvim_buf_set_lines(bufnr, 0, -1, true, content)
  end

  if api.nvim_buf_is_valid(bufnr) then
    api.nvim_buf_set_option(bufnr, 'modifiable', false)
    api.nvim_buf_set_option(bufnr, 'bufhidden', 'wipe')
  end

  local winid = api.nvim_open_win(bufnr, enter, opts)
  api.nvim_win_set_option(winid, 'winhl', 'Normal:LspFloatWinNormal,FloatBorder:' .. highlight)
  api.nvim_win_set_option(winid, 'winblend', content_opts.winblend or 0)

  -- disable winbar in some saga's floatwindow
  -- if config.symbol_in_winbar.enable or false then
  api.nvim_win_set_option(winid, 'winbar', '')
  -- end

  return bufnr, winid
end

function generate_spe_line(width)
  local char = '─'
  local line = ''
  for _ = 1, width, 1 do
    line = line .. char
  end
  return line
end

function wrap_text(text, width)
  local ret = {}

  if #text <= width then
    table.insert(ret, text)
    return ret
  end

  local tbl = vim.tbl_filter(function(a)
    return #a ~= 0
  end, vim.split(text, '%s'))

  if #tbl == 1 then
    if tbl[1]:find('──') then
      table.insert(ret, generate_spe_line(width))
      return ret
    end
  end

  local start_index, length = 1, 1

  for i = 1, #tbl do
    length = length + #tbl[i] + 1
    if length == width then
      table.insert(ret, table.concat(tbl, space, start_index, i))
      start_index = i + 1
      length = 0
    end

    if length > width and length - #tbl[i] <= width then
      table.insert(ret, table.concat(tbl, space, start_index, i - 1))
      start_index = i
      length = 0
    end

    if length < width and i == #tbl then
      table.insert(ret, table.concat(tbl, space, start_index, i))
    end
  end

  return ret
end

function wrap_diagnostic_msg(msg, width)
  if msg:find('\n') then
    local t = vim.tbl_filter(function(s)
      return string.len(s) ~= 0
    end, vim.split(msg, '\n'))
    return t
  end

  if #msg < width then
    return { msg }
  end

  return wrap_text(msg, width)
end

function wrap_add_truncate_line(contents)
  local line_widths = {}
  local width = 0
  local char = '─'
  local truncate_line = char

  for i, line in ipairs(contents) do
    line_widths[i] = vim.fn.strdisplaywidth(line)
    width = math.max(line_widths[i], width)
  end

  for _ = 1, width, 1 do
    truncate_line = truncate_line .. char
  end

  return truncate_line
end

function nvim_close_valid_window(winid)
  if winid == nil then
    return
  end

  local close_win = function(win_id)
    if not winid or win_id == 0 then
      return
    end
    if vim.api.nvim_win_is_valid(win_id) then
      vim.api.nvim_win_close(win_id, true)
    end
  end

  local _switch = {
    ['table'] = function()
      for _, id in ipairs(winid) do
        close_win(id)
      end
    end,
    ['number'] = function()
      close_win(winid)
    end,
  }

  local _switch_metatable = {
    __index = function(_, t)
      error(string.format('Wrong type %s of winid', t))
    end,
  }

  setmetatable(_switch, _switch_metatable)

  _switch[type(winid)]()
end

function close_preview_autocmd(bufnr, winids, events)
  vim.api.nvim_create_autocmd(events, {
    group = diag_augroup,
    buffer = bufnr,
    once = true,
    callback = function()
      nvim_close_valid_window(winids)
    end,
  })
end

function generate_empty_table(length)
  local empty_tbl = {}
  if length == 0 then
    return empty_tbl
  end

  for _ = 1, length do
    table.insert(empty_tbl, '   ')
  end
  return empty_tbl
end

function render_diagnostic_window(entry, option)
  local api = vim.api

  local diag_headers = { ' ', ' ', ' ', ' ' }
  local diag_types   = { 'Error', 'Warn', 'Info', 'Hint' }

  option = option or {}
  local current_buffer = api.nvim_get_current_buf()
  local wrap_message = {}
  local max_width = get_max_float_width()

  local severity = entry.severity

  local diag_header = diag_headers[severity]
  local diag_type = diag_types[severity]

  local source = ' '

  -- remove dot in source tail {lua-language-server}
  if entry.source and entry.source:find('%.$') then
    entry.source = entry.source:gsub('%.', '')
  end

  if entry.source then
    source = source .. entry.source
  end

  if entry.code ~= nil then
    source = source .. '(' .. entry.code .. ')'
  end

  local header_with_type = diag_header .. diag_type
  local lnum_col = ' in ' .. '❮' .. entry.lnum + 1 .. ':' .. entry.col + 1 .. '❯'
  wrap_message[1] = header_with_type .. lnum_col .. ' '

  local msgs = wrap_diagnostic_msg(entry.message, max_width)
  for _, v in pairs(msgs) do
    table.insert(wrap_message, v)
  end
  wrap_message[#wrap_message] = wrap_message[#wrap_message] .. source

  local truncate_line = wrap_add_truncate_line(wrap_message)
  table.insert(wrap_message, 2, truncate_line)

  local hi_name = 'FloatingDiagnostic' .. diag_type
  local content_opts = {
    contents = wrap_message,
    filetype = 'plaintext',
    highlight = hi_name,
  }

  local opts = {
    relative = 'cursor',
    style = 'minimal',
    move_col = 3,
  }

  local bufnr, winid = create_win_with_border(content_opts, opts)
  local win_config = api.nvim_win_get_config(winid)

  local above = win_config['row'][false] < vim.fn.winline()

  if win_config['anchor'] == 'NE' then
    opts.move_col = -1
  elseif win_config['anchor'] == 'NW' then
    opts.move_col = nil
  elseif win_config['anchor'] == 'SE' then
    opts.move_col = -2
  elseif win_config['anchor'] == 'SW' then
    opts.move_col = nil
  end

  opts.focusable = false

  local virt_bufnr, virt_winid = create_win_with_border({
    contents = generate_empty_table(#wrap_message),
    border = 'none',
  }, opts)

  local title_icon_length = #diag_header + #diag_type + 1
  api.nvim_buf_add_highlight(bufnr, -1, hi_name, 0, 0, title_icon_length)

  local truncate_line_hl = 'Diagnostic' .. diag_type
  api.nvim_buf_add_highlight(bufnr, -1, truncate_line_hl, 1, 0, -1)

  local get_pos_with_char = function()
    if win_config['anchor'] == 'NE' then
      return { 'right_align', '━', '┛' }
    end

    if win_config['anchor'] == 'NW' then
      return { 'overlay', '┗', '━' }
    end

    if win_config['anchor'] == 'SE' then
      return { 'right_align', '━', '┓' }
    end

    if win_config['anchor'] == 'SW' then
      return { 'overlay', '┏', '━' }
    end
  end

  local pos_char = get_pos_with_char()

  for i, _ in pairs(wrap_message) do
    local virt_tbl = {}
    if i > 2 then
      api.nvim_buf_add_highlight(bufnr, -1, hi_name, i - 1, 0, -1)
    end

    if not above then
      if i == #wrap_message then
        table.insert(virt_tbl, { pos_char[2], hi_name })
        table.insert(virt_tbl, { '━', hi_name })
        table.insert(virt_tbl, { pos_char[3], hi_name })
      else
        table.insert(virt_tbl, { '┃', hi_name })
      end
    else
      if i == 1 then
        table.insert(virt_tbl, { pos_char[2], hi_name })
        table.insert(virt_tbl, { '━', hi_name })
        table.insert(virt_tbl, { pos_char[3], hi_name })
      else
        table.insert(virt_tbl, { '┃', hi_name })
      end
    end

    api.nvim_buf_set_extmark(virt_bufnr, virt_ns, i - 1, 0, {
      id = i + 1,
      virt_text = virt_tbl,
      virt_text_pos = pos_char[1],
      virt_lines_above = false,
    })
  end

  api.nvim_buf_add_highlight(
    bufnr,
    -1,
    'DiagnosticLineCol',
    0,
    #header_with_type,
    #header_with_type + #lnum_col + 1
  )

  api.nvim_buf_add_highlight(
    bufnr,
    -1,
    'Comment',
    #wrap_message - 1,
    #wrap_message[#wrap_message] - #source,
    -1
  )

  api.nvim_buf_add_highlight(
    bufnr,
    -1,
    'DiagnosticMap',
    0,
    #wrap_message[1] + 12,
    -1
  )

  local close_autocmds = { 'CursorMoved', 'CursorMovedI', 'InsertEnter' }
  -- magic to solved the window disappear when trigger CusroMoed
  -- see https://github.com/neovim/neovim/issues/12923
  vim.defer_fn(function()
    close_preview_autocmd(current_buffer, { winid, virt_winid }, close_autocmds)
  end, 0)
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


local function set_keymap(...) vim.api.nvim_set_keymap(...) end

local function dump(o)
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
  local language_debuggers_adapters = {}
  local language_debuggers_settings = {}
  for _, val in pairs(t) do
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
    if type(val["debuggers"]) == "table" then
      if type(val["debuggers"]["adapters"]) == "table" then
        for k, v in pairs(val.debuggers.adapters) do
          language_debuggers_adapters[k] = v
        end
      end
      if type(val["debuggers"]["settings"]) == "function" then
        table.insert(language_debuggers_settings, val.debuggers.settings)
      end
    end
  end
  return { tools = language_tools, servers = language_servers, debuggers_adapters = language_debuggers_adapters, debuggers_settings = language_debuggers_settings }
end

local function set_common_settings()
  vim.g.mapleader = " "
  vim.g.netrw_browse_split = 0
  vim.g.netrw_banner = 0
  vim.g.netrw_winsize = 25

  vim.o.foldcolumn = '0'
  vim.o.foldlevel = 99
  vim.o.foldlevelstart = 99
  vim.o.foldenable = true

  vim.opt.guicursor = ""

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
end

set_common_settings()

require'nvim-web-devicons'.setup { }

vim.o.background = "light"

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

vim.cmd[[colorscheme tokyonight]]

require('lualine').setup {
  options = {
    theme = 'tokyonight'
  }
}

--vim.o.statuscolumn = "%@v:lua.ScFa@%C%T%@v:lua.ScLa@%s%T@v:lua.ScNa@%=%{v:lua.ScLn()}%T"
require('statuscol').setup({
  separator = true,
  relculright = true,
  setopt = false,
  order = "FSNs" -- fold, sign, line number, separator
})

require("no-neck-pain").setup({
    enableOnVimEnter = true,
    width = 180,
    buffers = {
        blend = 0,
    },
})

local nvim_runtime_path = vim.split(package.path, ';')
table.insert(nvim_runtime_path, 'lua/?.lua')
table.insert(nvim_runtime_path, 'lua/?/init.lua')

local languages = {
  others = {
    tools = { },
    debuggers = { },
    servers = { }
  },
  csharp = {
    tools = {},
    debuggers = {
      adapters = {
        coreclr = function (dap)
          dap.adapters.coreclr = {
            type = 'executable',
            command = 'netcoredbg',
            args = {'--interpreter=vscode'}
          }
        end,
      },
      settings = function (dap)
        dap.configurations.cs = {
          {
            type = "coreclr",
            name = "launch - netcoredbg",
            request = "launch",
            program = function()
                return vim.fn.input('Path to dll', vim.fn.getcwd(), 'file')
            end,
          },
        }
      end,
    },
    servers = {
      omnisharp = { use_mono = false }
    }
  },
  fsharp = {
    tools = { fantomas = {} },
    debuggers = {
      adapters = {
        coreclr = function (dap)
          dap.adapters.coreclr = {
            type = 'executable',
            command = 'netcoredbg',
            args = {'--interpreter=vscode'}
          }
        end,
      },
      settings = function (dap)
        dap.configurations.fsharp = {
          {
            type = "coreclr",
            name = "launch - netcoredbg",
            request = "launch",
            program = function()
                return vim.fn.input('Path to dll', vim.fn.getcwd(), 'file')
            end,
          },
        }
      end,
    },
    servers = {
      fsautocomplete = {
        on_attach = function(client, bufnr)
          vim.cmd [[
            setl fdm=syntax
            setl formatoptions=croql
            setl commentstring=(*%s*)
            setl comments=s0:*\ -,m0:*\ \ ,ex0:*),s1:(*,mb:*,ex:*),:\/\/\/,:\/\/
            setl indentkeys+=0=and,0=class,0=constraint,0=done,0=else,0=end,0=exception,0=external,0=if,0=in,0=include,0=inherit,0=let,0=method,0=open,0=then,0=type,0=val,0=with,0;;,0>\],0\|\],0>},0\|,0},0\],0)
          ]]
        end,
        cmd = { "fsautocomplete" },
        filetypes = { "fsharp" },
        init_options = {
          AutomaticWorkspaceInit = true
        }
      }
    }
  },
  jsts = {
    tools = {},
    debuggers = {},
    servers = {
      tsserver = {
        on_attach = function(client, bufnr)
        end,
      }
    }
  },
  lua = {
    servers = {
      lua_ls = {
        settings = {
          Lua = {
            runtime = {
              version = 'LuaJIT',
              path = nvim_runtime_path,
            },
            diagnostics = {
              globals = { 'vim' },
            },
            workspace = {
              checkThirdParty = false,
              library = vim.api.nvim_get_runtime_file('', true)
              },
            telemetry = { enable = false, },
          }
        }
      }
    }
  },
  rust = {
    debuggers = {
      adapters = {
        codelldb = function (dap)
          dap.adapters.codelldb = {
            type = 'server',
            port = "${port}",
            executable = {
              command = 'codelldb',
              args = {"--port", "${port}"},
            }
          }
        end,
      },
      settings = function (dap)
        dap.configurations.rust = {
          {
            name = "Launch...",
            type = "codelldb",
            request = "launch",
            program = function()
              return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
            end,
            cwd = '${workspaceFolder}',
            stopOnEntry = false,
          },
        }
      end,
    },
    servers = {
      rust_analyzer = {
        imports = {
            granularity = {
                group = "module",
            },
            prefix = "self",
        },
        cargo = {
            buildScripts = {
                enable = true,
            },
        },
        procMacro = {
            enable = true,
        },
        inlayHints = {
            enabled = false,
            typeHints = {
                enable = false,
            },
        },
      }
    }
  },
  yaml = {
    tools = { yamllint = {}, yamlfmt = {} },
    servers = {
      yamlls = {}
    }
  }
}

local function common_capabilities()
  local status_ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")

  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem = {
    documentationFormat = { "markdown", "plaintext" },
    snippetSupport = true,
    preselectSupport = true,
    insertReplaceSupport = true,
    labelDetailsSupport = true,
    deprecatedSupport = true,
    commitCharactersSupport = true,
    tagSupport = { valueSet = { 1 } },
    resolveSupport = {
      properties = {
        "documentation",
        "detail",
        "additionalTextEdits",
      },
    },
  }

  capabilities.textDocument.foldingRange = {
    dynamicRegistration = false,
    lineFoldingOnly = true
  }

  if status_ok then
    return cmp_nvim_lsp.default_capabilities(capabilities)
  end

  return capabilities
end

local function configure_handlers(telescope_builtin)
  local config = {
    signs = {
      active = true,
      values = {
        { name = "DiagnosticSignError", text = "" },
        { name = "DiagnosticSignWarn", text = "" },
        { name = "DiagnosticSignHint", text = "" },
        { name = "DiagnosticSignInfo", text = "" },
      },
    },
    virtual_text = true,
    virtual_lines = true,
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
  --vim.lsp.handlers["textDocument/hover"] = hover_handler
  vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, config.float)
  vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, config.float)
  vim.lsp.handlers['textDocument/declaration'] = vim.lsp.with(vim.lsp.buf.declaration, config.float)
  vim.lsp.handlers['textDocument/definition'] = telescope_builtin.lsp_definitions
  vim.lsp.handlers['textDocument/documentSymbol'] = telescope_builtin.lsp_document_symbols
  vim.lsp.handlers['workspace/symbol'] = telescope_builtin.lsp_workspace_symbols
  vim.lsp.handlers['textDocument/references'] = telescope_builtin.lsp_references
  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
      virtual_text = config.virtual_text,
      update_in_insert = true,
      show_diagnostic_autocmds = { 'InsertLeave', 'TextChanged', "TextYankPost", "CursorHold" },
    }
  )
  --vim.lsp.handlers['textDocument/implementation'] = location_handler('LSP Implementations', opts.location),
  --vim.lsp.handlers['textDocument/typeDefinition'] = location_handler('LSP Type Definitions', opts.location),
  --vim.lsp.handlers['workspace/symbol'] = symbol_handler('LSP Workspace Symbols', opts.symbol),
  --vim.lsp.handlers['callHierarchy/incomingCalls'] = call_hierarchy_handler('LSP Incoming Calls', 'from', opts.call_hierarchy),
  --vim.lsp.handlers['callHierarchy/outgoingCalls'] = call_hierarchy_handler('LSP Outgoing Calls', 'to', opts.call_hierarchy),
  --vim.lsp.handlers['textDocument/codeAction'] = code_action_handler('LSP Code Actions', opts.code_action)
end

local lsp_keybinds = function(bufnr)
  local opts = { noremap = false, silent = true }
  vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<cmd>lua hover_handler()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<C-h>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>ds", "<cmd>lua vim.lsp.buf.document_symbol()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "i", "<C-;>", "<cmd>lua vim.lsp.buf.document_symbol()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>ws", "<cmd>lua vim.lsp.buf.workspace_symbol('')<CR>", opts)
  --vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<CMD>Glance definitions<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<CMD>Glance references<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gy', '<CMD>Glance type_definitions<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gm', '<CMD>Glance implementations<CR>', opts)
  --vim.api.nvim_buf_set_keymap(bufnr, "n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gI", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  --vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gl", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gs", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "i", "<C-.>", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
end

local tooling = get_tooling(languages)

local telescope = require('telescope')
local telescope_actions = require('telescope.actions')
local telescope_builtin = require('telescope.builtin')

telescope.setup {
  defaults = {
    layout_strategy = 'flex',
    layout_config = {
      width = 0.75,
      height = 0.95
    },
    file_sorter = require("telescope.sorters").get_fuzzy_file,
    generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
    vimgrep_arguments = {
      "rg",
      "--color=never",
      "--no-heading",
      "--with-filename",
      "--line-number",
      "--column",
      "--smart-case",
      "--trim"
    },
    find_command = { "fdfind", "--type", "f", "--strip-cwd-prefix" },
    file_ignore_patterns = {
        ".git/",
        ".vscode/",
        "node_modules",
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
    buffers = {
      preview = true,
      only_cwd = false,
      show_all_buffers = false,
      ignore_current_buffer = true,
      sort_lastused = true,
      theme = "dropdown",
      sorter = require("telescope.sorters").get_substr_matcher(),
      selection_strategy = "closest",
      path_display = {"smart"},
      layout_strategy = "center",
      winblend = 0,
      layout_config = {width = 70},
    }
  },
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",
    },
    ["ui-select"] = {
      require("telescope.themes").get_dropdown {
        layout_strategy = "cursor",
      }
    },
    aerial = {
      show_nesting = {
        ['_'] = false, -- This key will be the default
        json = true,
        yaml = true,
      }
    }
  }
}

telescope.load_extension("fzf")
telescope.load_extension("ui-select")
telescope.load_extension("undo")
telescope.load_extension('aerial')

project_files = function()
  local opts = {} -- define here if you want to define something
  local ok = pcall(telescope_builtin.git_files, opts)
  if not ok then
    telescope_builtin.find_files(opts)
  end
end

set_keymap('n', '<leader>/', ':Telescope current_buffer_fuzzy_find<CR>', { noremap = true, silent= true })
set_keymap('n', '<C-s>', ':Telescope current_buffer_fuzzy_find<CR>', { noremap = true, silent= true })
set_keymap('n', '<C-f>', '<CMD>lua project_files()<CR>', { noremap = true, silent= true })
set_keymap('n', '<M-s>', ':Telescope live_grep<CR>', { noremap = true, silent= true })
set_keymap('n', '<C-b>', ':Telescope buffers<CR>', { noremap = true, silent= true })
set_keymap('n', '<C-u>', ':Telescope undo<CR>', { noremap = true, silent= true })

local mason = require('mason')
local mason_lspconfig = require('mason-lspconfig')
local nvim_lsp = require('lspconfig')
local mason_tool_installer = require('mason-tool-installer')
local mason_nvim_dap = require('mason-nvim-dap')
local null_ls = require('null-ls')
local barbecue = require('barbecue')
local barbecue_ui = require("barbecue.ui")
local navic = require('nvim-navic')
local nvim_cmp = require('cmp')
local luasnip = require('luasnip')
local treesitter = require('nvim-treesitter.configs')
local treesitter_context = require('treesitter-context')
local treesitter_parsers = require('nvim-treesitter.parsers')
local treesitter_highlighter = require('vim.treesitter.highlighter')
local illuminate = require('illuminate')
local inlay_hints = require("lsp-inlayhints")
local dap = require('dap')
local dapui = require('dapui')
local dapvt = require("nvim-dap-virtual-text")
local glance = require('glance')
local glance_actions = glance.actions

dapui.setup()
dapvt.setup()

dap.listeners.after.event_initialized["dapui_config"] = function()
  dapui.open()
end
dap.listeners.before.event_terminated["dapui_config"] = function()
  dapui.close()
end
dap.listeners.before.event_exited["dapui_config"] = function()
  dapui.close()
end

require("neotest").setup({
  adapters = {
    require("neotest-dotnet")({
      dap = { justMyCode = false },
      custom_attributes = {
        xunit = { "DirectoryAttribute" },
      },
    })
  }
})

local icons = {
    File = "[f]",
    Module = "[M]",
    Namespace = "[n]",
    Package = "[P]",
    Class = "[C]",
    Method = "[m]",
    Property = "[p]",
    Field = "[F]",
    Constructor = "[ctor]",
    Enum = "[enum]",
    Interface = "[I]",
    Function = "[f]",
    Variable = "[var]",
    Constant = "[const]",
    String = "[s]",
    Number = "[n]",
    Boolean = "[bool]",
    Array = "[]",
    Object = "[obj]",
    Key = "[k]",
    Null = "[nil]",
    EnumMember = "[enum.M]",
    Struct = "[s]",
    Event = "[e]",
    Operator = "[op]",
    TypeParameter = "[T]",
}

navic.setup  {
  icons = icons,
  separator = " / ",
  depth_limit_indicator = "..",
}

barbecue.setup({
  theme = 'tokyonight',
  create_autocmd = false,
  show_modified = true,
  attach_navic = false,
  kinds = icons,
  symbols = {
    modified = "*",
    ellipsis = "...",
    separator = "->",
  },
})

vim.api.nvim_create_autocmd({
  "WinScrolled",
  "BufWinEnter",
  "CursorHold",
  "InsertLeave",
  "BufWritePost",
  "TextChanged",
  "TextChangedI",
}, {
  group = vim.api.nvim_create_augroup("barbecue", {}),
  callback = function()
    barbecue_ui.update()
  end,
})

inlay_hints.setup({
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

treesitter.setup {
  -- TODO: Add this to overall langauges config, per language.
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

treesitter_context.setup({
  enable = false, -- TODO: Broken now, re-enable later
  max_lines = 0,
})

treesitter_parsers.filetype_to_parsername.octo = "markdown"

treesitter_parsers.get_parser_configs().fsharp = {
  install_info = {
    url = "~/code/tree-sitter-fsharp",
    -- branch = "develop",
    files = {"src/scanner.cc", "src/parser.c" }
  },
  filetype = "fsharp",
}

require "nvim-treesitter.configs".setup {
  playground = {
    enable = true,
    disable = {},
    updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
    persist_queries = false, -- Whether the query persists across vim sessions
    keybindings = {
      toggle_query_editor = 'o',
      toggle_hl_groups = 'i',
      toggle_injected_languages = 't',
      toggle_anonymous_nodes = 'a',
      toggle_language_display = 'I',
      focus_language = 'f',
      unfocus_language = 'F',
      update = 'R',
      goto_node = '<cr>',
      show_help = '?',
    },
  }
}

local capabilities = common_capabilities()

local fidget = require('fidget')

fidget.setup({
  text = {
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
    relative = "editor" -- win or editor
  }
})

require('nvim-autopairs').setup {
  disable_filetype = { "TelescopePrompt" },
}

require('copilot').setup({
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

local has_words_before = function()
  if vim.api.nvim_buf_get_option(0, "buftype") == "prompt" then return false end
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_text(0, line-1, 0, line-1, col, {})[1]:match("^%s*$") == nil
end
vim.opt.completeopt = {'menu', 'menuone', 'noselect'}

require("copilot_cmp").setup {
  method = "getCompletionsCycling",
  formatters = {
    label = require("copilot_cmp.format").format_label_text,
    insert_text = require("copilot_cmp.format").format_insert_text,
    preview = require("copilot_cmp.format").deindent,
  },
}

nvim_cmp.setup {
  view = {
    entries = "custom",
    selection_order = 'near_cursor'
  },
  enabled = function()
    -- disable completion in comments
    local context = require 'cmp.config.context'
    -- keep command mode completion enabled when cursor is in a comment
    if vim.api.nvim_get_mode().mode == 'c' then
      return true
    else
      return not context.in_treesitter_capture("comment")
        and not context.in_syntax_group("Comment")
    end
  end,
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = {
    ['<CR>']      = nvim_cmp.mapping.confirm({
      behavior = nvim_cmp.ConfirmBehavior.Replace,
      select = false
    }),
    ['<C-p>']     = nvim_cmp.mapping.select_prev_item(),
    ['<C-n>']     = nvim_cmp.mapping.select_next_item(),
    ['<Up>']      = nvim_cmp.mapping.select_prev_item(),
    ['<Down>']    = nvim_cmp.mapping.select_next_item(),
    ['<C-d>']     = nvim_cmp.mapping.scroll_docs(-4),
    ['<C-f>']     = nvim_cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = nvim_cmp.mapping.complete(),
    ['<C-e>']     = nvim_cmp.mapping.close(),
    ['<Tab>']     = function(fallback)
      if nvim_cmp.visible() and has_words_before() then
        nvim_cmp.select_next_item({ behavior = nvim_cmp.SelectBehavior.Select })
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end,
    ['<S-Tab>'] = function(fallback)
      if nvim_cmp.visible() and has_words_before() then
        nvim_cmp.select_prev_item({ behavior = nvim_cmp.SelectBehavior.Select })
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end,
  },
  sources = nvim_cmp.config.sources({
    { name = "copilot" },
    { name = 'nvim_lsp_signature_help' },
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = 'git' }
  },{
    { name = 'buffer' },
    { name = 'calc' },
    { name = "crates" },
  }),
  window = {
    completion = {
      winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,Search:None",
      col_offset = -3,
      side_padding = 0,
    },
  },
  formatting = {
    fields = { "kind", "abbr", "menu" },
    insert_text = require("copilot_cmp.format").remove_existing,
    format = function(entry, vim_item)
      local kind = require("lspkind").cmp_format({ mode = "symbol_text", maxwidth = 50, symbol_map = { Copilot = "" } })(entry, vim_item)
      local strings = vim.split(kind.kind, "%s", { trimempty = true })
      kind.kind = " " .. strings[1] .. " "
      kind.menu = "    (" .. strings[2] .. ")"

      return kind
    end,
  },
  sorting = {
    priority_weight = 2,
    comparators = {
      require("copilot_cmp.comparators").prioritize,
      require("copilot_cmp.comparators").score,

      -- Below is the default comparitor list and order for nvim-cmp
      nvim_cmp.config.compare.offset,
      -- cmp.config.compare.scopes, --this is commented in nvim-cmp too
      nvim_cmp.config.compare.exact,
      nvim_cmp.config.compare.score,
      nvim_cmp.config.compare.recently_used,
      nvim_cmp.config.compare.locality,
      nvim_cmp.config.compare.kind,
      nvim_cmp.config.compare.sort_text,
      nvim_cmp.config.compare.length,
      nvim_cmp.config.compare.order,
    },
  },
}

nvim_cmp.setup.filetype('gitcommit', {
  sources = nvim_cmp.config.sources({
    { name = 'cmp_git' },
  }, {
    { name = 'buffer' },
  })
})

nvim_cmp.setup.cmdline({ '/', '?' }, {
  mapping = nvim_cmp.mapping.preset.cmdline(),
  sources = nvim_cmp.config.sources({
    { name = 'nvim_lsp_document_symbol' }
  }, {
    { name = 'buffer' }
  }),
  view = { entries = "native" },
})

nvim_cmp.setup.cmdline(':', {
  mapping = nvim_cmp.mapping.preset.cmdline(),
  sources = nvim_cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  }),
  view = { entries = "native" },
})

if vim.o.ft == 'clap_input' and vim.o.ft == 'guihua' and vim.o.ft == 'guihua_rust' then
  nvim_cmp.setup.buffer { completion = {enable = false} }
end

nvim_cmp.event:on("menu_opened", function ()
  vim.b.copilot_suggestion_hidden = true
end)

nvim_cmp.event:on("menu_closed", function ()
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

mason.setup({
    PATH = "prepend",
    max_concurrent_installers = 10,
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
    ensure_installed = vim.tbl_keys(tooling.debuggers_adapters),
    automatic_installation = true,
    automatic_setup = true
})

null_ls.setup({
    sources = {
        null_ls.builtins.code_actions.gitsigns,
        null_ls.builtins.completion.luasnip,
        null_ls.builtins.completion.tags,
        null_ls.builtins.hover.dictionary,
        null_ls.builtins.hover.printenv,
    },
})

local lsp_formatting = function(bufnr)
    vim.lsp.buf.format({
        filter = function(client)
            return client.name == "null-ls"
        end,
        bufnr = bufnr,
    })
end

local setup_dap = function(idap)
  for _, cb in pairs(tooling.debuggers_adapters) do
    if type(cb) == "function" then
      cb(idap)
    end
  end
  for _, cb in pairs(tooling.debuggers_settings) do
    if type(cb) == "function" then
      cb(idap)
    end
  end
end

setup_dap(dap)

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
    enable = true, -- Will generate colors for the plugin based on your current colorscheme
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
      local uri = vim.uri_from_bufnr(0)
      if #results == 1 then
        local target_uri = results[1].uri or results[1].targetUri
        if target_uri == uri then
          jump(results[1])
        else
          open(results)
        end
      else
        open(results)
      end
    end,
  },
  folds = {
    fold_closed = '',
    fold_open = '',
    folded = true, -- Automatically fold list on startup
  },
  indent_lines = {
    enable = true,
    icon = '│',
  },
  winbar = {
    enable = true, -- Available strating from nvim-0.8+
  },
})

local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

configure_handlers(telescope_builtin)

hover_handler = function(client, bufnr)
  local opts = { focus=false, scope="cursor" }
  local winid = require('ufo').peekFoldedLinesUnderCursor()
  if not winid then
      local pos = vim.api.nvim_win_get_cursor(0)
      local line_nr = pos[1] - 1
      local column_nr = pos[2]
      local diagnostic_under_cursor =
        vim.tbl_filter(in_range(line_nr, column_nr), vim.diagnostic.get(bufnr, client))

      if rawequal(next(diagnostic_under_cursor), nil) then
        vim.lsp.buf.hover(nil, opts)
      else
        -- vim.diagnostic.open_float(nil, opts)
        render_diagnostic_window(diagnostic_under_cursor[0] or diagnostic_under_cursor[1], opts)
      end
  end
end

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

local global_on_attach = function(client, bufnr)
    if client.server_capabilities.documentSymbolProvider then
        navic.attach(client, bufnr)
    end

    if client.supports_method("textDocument/formatting") then
        vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
        vim.api.nvim_create_autocmd("BufWritePre", {
            group = augroup,
            buffer = bufnr,
            callback = function()
                lsp_formatting(bufnr)
            end,
        })
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
      vim.api.nvim_create_autocmd("CursorHold, CursorHoldI", {
        buffer = bufnr,
        callback = function() hover_handler(client, bufnr) end
      })
    end

    client.server_capabilities.documentFormattingProvider = false
    client.server_capabilities.documentRangeFormattingProvider = false

    inlay_hints.on_attach(client, bufnr, false)

    lsp_keybinds(bufnr)
end

illuminate.configure({
  providers = {
        'lsp',
        'treesitter',
        'regex',
    },
  delay = 50
})

mason_lspconfig.setup_handlers {
    function (server_name)
        local server_opts = tooling.servers[server_name] or {}
        local server_on_attach = server_opts["on_attach"] or function(_, _) end
        local server_settings = server_opts["settings"] or {}
        local server_capabilities = server_opts["capabilities"] or {}

        local composed_on_attach_fn = function(client, bufnr)
          server_on_attach(client, bufnr)
          global_on_attach(client, bufnr)
        end

        local opts = {
          on_attach = composed_on_attach_fn,
          capabilities = merge(capabilities, server_capabilities),
	        settings = server_settings,
          flags = {
            debounce_text_changes = 150,
          }
        }

        nvim_lsp[server_name].setup(opts)
    end,
}

require'sniprun'.setup({
  display = {
    "VirtualText",
  },
  live_display = { "VirtualTextOk" },
  interpreter_options = {
    FSharp_fifo = {
      interpreter = "dotnet fsi --nologo --langversion:preview"
    }
  }
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

local neogit = require('neogit')
neogit.setup {
  integrations = {
    diffview = true
  },
}

require"octo".setup({
  default_remote = {"upstream", "origin"}
})

require("diffview").setup({
  enhanced_diff_hl = true
})

local aerial = require('aerial')
aerial.setup({
  on_attach = function(bufnr)
    aerial.open_all()
  end,
  backends = { "treesitter", "lsp", "markdown", "man" },
  -- close_automatic_events = { "unfocus", "switch_buffer", "unsupported" },
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
      local padding = 0
      conf.anchor = 'NE'
      conf.row = padding
      conf.col = vim.o.columns - padding
      return conf
    end,
  }
})

require('crates').setup({
  null_ls = {
        enabled = true,
        name = "crates.nvim",
    },
})

require('ufo').setup({
  open_fold_hl_timeout = 150,
  close_fold_kinds = {'imports', 'comment'},
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
  provider_selector = function(bufnr, filetype, buftype)
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

require('nvim_context_vt').setup({
  enabled = true
})

require("oil").setup({
  columns = {
    "icon",
    "permissions",
    "size",
    "mtime",
  },
  buf_options = {
    buflisted = false,
  },
  win_options = {
    wrap = false,
    signcolumn = "no",
    cursorcolumn = false,
    foldcolumn = "0",
    spell = false,
    list = false,
    conceallevel = 3,
    concealcursor = "n",
  },
  restore_win_options = true,
  skip_confirm_for_simple_edits = false,
  keymaps = {
    ["g?"] = "actions.show_help",
    ["<CR>"] = "actions.select",
    ["<C-s>"] = "actions.select_vsplit",
    ["<C-h>"] = "actions.select_split",
    ["<C-t>"] = "actions.select_tab",
    ["<C-p>"] = "actions.preview",
    ["<C-c>"] = "actions.close",
    ["<C-l>"] = "actions.refresh",
    ["-"] = "actions.parent",
    ["_"] = "actions.open_cwd",
    ["`"] = "actions.cd",
    ["~"] = "actions.tcd",
    ["g."] = "actions.toggle_hidden",
  },
  use_default_keymaps = true,
  view_options = {
    show_hidden = false,
  },
  float = {
    padding = 2,
    max_width = 0,
    max_height = 0,
    border = "rounded",
    win_options = {
      winblend = 10,
    },
  },
})

