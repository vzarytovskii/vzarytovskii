local awful = require("awful")
local xresources = require("beautiful.xresources")

local theme_path = os.getenv("HOME") .. "/.config/awesome/theme"
local theme_wallpaper = theme_path .. "/wall.jpg"
local dpi = xresources.apply_dpi

-- | Definition  | --
base_color = "#000000"
secondary_color = "#00FFFF"
opacity_hex = "FF"

-- | THEME | --
theme                                           = {}
theme.theme_path                                = theme_path
theme.wallpaper					= theme_wallpaper
theme.icon_theme                                = "Papirus-Adapta-Nokto"
theme.font                                      = "Hack 10"

-- | Base | --
theme.bg_normal                                 = base_color..opacity_hex
theme.bg_focus                                  = theme.bg_normal
theme.bg_minimize                               = theme.bg_normal
theme.bg_urgent                                 = base_color

theme.fg_normal                                 = "#FFFFFF"
theme.fg_focus                                  = secondary_color
theme.fg_urgent                                 = "#e0c625"
theme.fg_minimize                               = "#15abc3"

-- | Systray | --
theme.bg_systray                                = theme.bg_normal
theme.systray_icon_spacing                      = dpi(0)

-- | Borders | --
theme.useless_gap                               = dpi(0)
theme.border_width                              = dpi(1)
theme.border_normal                             = "#333333"
theme.border_focus                              = "#777777"
theme.border_marked                             = "#FFFFFF"

-- | Notification | --
theme.notification_fg                           = "#6F6F6F"
theme.notification_bg                           = "#FFFFFF"
theme.notification_border_color                 = secondary_color
theme.notification_border_width                 = 1
theme.notification_max_height                   = 300
theme.notification_width                        = 300
theme.notification_icon_size                    = 30

-- | Hotkeys help | --
theme.hotkeys_modifiers_fg                      = "#204143"
theme.hotkeys_border_color                      = secondary_color

-- | Calendar | --
theme.calendar_month_bg_color                   = base_color
theme.calendar_year_bg_color                    = base_color

-- | Tasklist | --
theme.tasklist_bg_normal                        = base_color
theme.tasklist_fg_normal                        = theme.fg_normal
theme.tasklist_fg_focus				= theme.tasklist_fg_normal
theme.tasklist_bg_focus                         = secondary_color.."11" 
theme.tasklist_disable_icon			= true

-- | Taglist squares | --
theme.taglist_squares_sel                       = theme.theme_path .. "/taglist/square_sel.png"
theme.taglist_squares_unsel                     = theme.theme_path .. "/taglist/square_unsel.png"
theme.taglist_fg_focus                          = theme.fg_normal
theme.taglist_bg_focus				= secondary_color.."11"
theme.taglist_font                              = "Hack 9"

-- | Titlebar | --
theme.titlebar_bg_focus                         = base_color

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
