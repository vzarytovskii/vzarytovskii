require('material').setup()
vim.g.material_style = "deep ocean"
vim.cmd 'colorscheme material'

require('lualine').setup {
  options = { 
    lower = true,
    icons_enabled = 1,
    padding = 1,
    left_padding = 1,
    right_padding = 1,
    theme = "material-stealth"
  },
  sections = { lualine_a = {{ 'mode', lower = false }}, lualine_b = { 'branch' }}
}

