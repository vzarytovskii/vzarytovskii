--[[

   Awesome WM Tron Legacy Theme 1.5
   Distopico Vegan <distopico [at] riseup [dot] net>
   Licensed under GPL3

--]]
local awful = require("awful")
local xresources = require("beautiful.xresources")

-- Theme basic --
local theme_path = os.getenv("HOME") .. "/.config/awesome/theme"
local theme_wallpaper = theme_path .. "/wall.jpg"
local dpi = xresources.apply_dpi

-- Load wallpaper from different locations --
local wall_directories = {
   os.getenv("HOME") .. "/Pictures/Wallpapers",
   os.getenv("HOME") .. "/Pictures"
}

for i=1, #wall_directories do
   local dir = wall_directories[i]

   if awful.util.file_readable(theme_path .. "/_wall.jpg") then
      theme_wallpaper = theme_path .. "/_wall.jpg"
      break
   end
   if awful.util.file_readable(dir .. "/wall.png") then
      theme_wallpaper = dir .. "/wall.png"
      break
   elseif awful.util.file_readable(dir .. "/wall.jpg") then
      theme_wallpaper = dir .. "/wall.jpg"
      break
   elseif awful.util.file_readable(dir .. "/wallpaper.png") then
      theme_wallpaper = dir .. "/wallpaper.png"
      break
   elseif awful.util.file_readable(dir .. "/wallpaper.jpg") then
      theme_wallpaper = dir .. "/wallpaper.jpg"
      break
   end
end

-- | Definition  | --
base_color = "#001214"
opacity_hex = "CC"

-- | THEME | --
theme                                           = {}
theme.theme_path                                = theme_path
theme.wallpaper                                 = theme_wallpaper
theme.icon_theme                                = "Papirus-Adapta-Nokto"
theme.font                                      = "Hack 8"

-- | Base | --
theme.bg_normal                                 = base_color..opacity_hex
theme.bg_focus                                  = theme.bg_normal
theme.bg_minimize                               = theme.bg_normal
theme.bg_urgent                                 = base_color

theme.fg_normal                                 = "#DDDDDD"
theme.fg_focus                                  = "#00FFFF"
theme.fg_urgent                                 = "#e0c625"
theme.fg_minimize                               = "#15abc3"

-- | Systray | --
theme.bg_systray                                = theme.bg_normal
theme.systray_icon_spacing                      = dpi(5)

-- | Borders | --
theme.useless_gap                               = dpi(5)
theme.border_width                              = dpi(2)
theme.border_normal                             = base_color
theme.border_focus                              = "#00FFFF"
theme.border_marked                             = "#FFFFFF"

-- | Notification | --
theme.notification_fg                           = "#6F6F6F"
theme.notification_bg                           = "#FFFFFF"
theme.notification_border_color                 = "#00FFFF"
theme.notification_border_width                 = 1
theme.notification_max_height                   = 300
theme.notification_width                        = 300
theme.notification_icon_size                    = 30

-- | Menu | --
theme.menu_bg_normal                            = base_color
theme.menu_bg_focus                             = theme.menu_bg_normal
theme.menu_icon                                 = theme.theme_path .. "/icons/menu.png"
theme.menu_submenu_icon                         = theme.theme_path .. "/icons/submenu.png"
theme.menu_height                               = dpi(16)
theme.menu_width                                = dpi(100)

-- | Hotkeys help | --
theme.hotkeys_modifiers_fg                      = "#204143"
theme.hotkeys_border_color                      = "#00FFFF"

-- | Calendar | --
theme.calendar_month_bg_color                   = base_color
theme.calendar_year_bg_color                    = base_color

-- | Tasklist | --
theme.tasklist_bg_normal                        = base_color.."00"
theme.tasklist_bg_focus                         = theme.tasklist_bg_normal
theme.tasklist_fg_normal                        = theme.fg_normal.."CC"

-- | Taglist squares | --
theme.taglist_squares_sel                       = theme.theme_path .. "/taglist/square_sel.png"
theme.taglist_squares_unsel                     = theme.theme_path .. "/taglist/square_unsel.png"
theme.taglist_fg_focus                          = "#00FFFF"
theme.taglist_font                              = "Icons 10"

-- | Titlebar | --
theme.titlebar_bg_focus                         = base_color

theme.titlebar_close_button_focus               = theme.theme_path .. "/titlebar/close_focus.png"
theme.titlebar_close_button_normal              = theme.theme_path .. "/titlebar/close_normal.png"

theme.titlebar_ontop_button_focus_active        = theme.theme_path .. "/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active       = theme.theme_path .. "/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive      = theme.theme_path .. "/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive     = theme.theme_path .. "/titlebar/ontop_normal_inactive.png"

theme.titlebar_sticky_button_focus_active       = theme.theme_path .. "/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active      = theme.theme_path .. "/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive     = theme.theme_path .. "/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive    = theme.theme_path .. "/titlebar/sticky_normal_inactive.png"

theme.titlebar_floating_button_focus_active     = theme.theme_path .. "/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active    = theme.theme_path .. "/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive   = theme.theme_path .. "/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive  = theme.theme_path .. "/titlebar/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active    = theme.theme_path .. "/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active   = theme.theme_path .. "/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = theme.theme_path .. "/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = theme.theme_path .. "/titlebar/maximized_normal_inactive.png"

-- | Separators | --
theme.spr1px                                    = theme.theme_path .. "/separators/spr1px.png"
theme.spr2px                                    = theme.theme_path .. "/separators/spr2px.png"
theme.spr4px                                    = theme.theme_path .. "/separators/spr4px.png"
theme.spr5px                                    = theme.theme_path .. "/separators/spr5px.png"
theme.spr10px                                   = theme.theme_path .. "/separators/spr10px.png"

-- | Layout | --
theme.layout_floating                           = theme.theme_path .. "/layouts/floating.png"
theme.layout_tile                               = theme.theme_path .. "/layouts/tile.png"
theme.layout_tileleft                           = theme.theme_path .. "/layouts/tileleft.png"
theme.layout_tilebottom                         = theme.theme_path .. "/layouts/tilebottom.png"
theme.layout_tiletop                            = theme.theme_path .. "/layouts/tiletop.png"
theme.layout_fairh                              = theme.theme_path .. "/layouts/fairh.png"
theme.layout_fairv                              = theme.theme_path .. "/layouts/fairv.png"
theme.layout_spiral                             = theme.theme_path .. "/layouts/spiral.png"
theme.layout_dwindle                            = theme.theme_path .. "/layouts/dwindle.png"
theme.layout_max                                = theme.theme_path .. "/layouts/max.png"
theme.layout_fullscreen                         = theme.theme_path .. "/layouts/fullscreen.png"
theme.layout_magnifier                          = theme.theme_path .. "/layouts/magnifier.png"
theme.layout_cornernw                           = theme.theme_path .. "/layouts/cornernw.png"
theme.layout_cornerne                           = theme.theme_path .. "/layouts/cornerne.png"
theme.layout_cornersw                           = theme.theme_path .. "/layouts/cornersw.png"
theme.layout_cornerse                           = theme.theme_path .. "/layouts/cornerse.png"

-- | Misc | --
theme.awesome_icon                              = theme.theme_path .. "/icons/awesome.png"
theme.awesome_icon_w                            = theme.theme_path .. "/icons/awesome_w.png"

return theme
