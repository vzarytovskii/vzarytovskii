require('lualine').setup {
  options = { 
    lower = true,
    icons_enabled = 0,
    padding = 1,
    left_padding = 1,
    right_padding = 1,
    theme = "16color"
  },
  sections = { lualine_a = {{ 'mode', lower = false }}, lualine_b = { 'branch' }}
}
