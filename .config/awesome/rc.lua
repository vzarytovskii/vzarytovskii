local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget
local cyclefocus = require("libs/cyclefocus")
local myvolume = require("widgets/volume")
local mybrightness = require("widgets/brightness")
local mybattery = require("widgets/battery")
local mywifi = require("widgets/wifi")

require("awful.autofocus")
require("awful.hotkeys_popup.keys")

if awesome.startup_errors then
   naughty.notify({ preset = naughty.config.presets.critical,
                    title = "Oops, there were errors during startup!",
                    text = awesome.startup_errors })
end

do
   local in_error = false
   awesome.connect_signal(
      "debug::error", function (err)
         if in_error then return end
         in_error = true

         naughty.notify({ preset = naughty.config.presets.critical,
                          title = "Oops, an error happened!",
                          text = tostring(err) })
         in_error = false
   end)
end
sloppy_focus = false
notify_suspended = false

home_path  = os.getenv("HOME") .. "/"
config_path = home_path .. ".config/awesome/"
beautiful.init(config_path .. "theme/theme.lua")

terminal = "alacritty" or "urxvtc" or "urxvt" or "terminator" or "gnome-terminal" or "xterm"
editor = os.getenv("EDITOR") or "emacs" or "vim" or "nano"
editor_cmd = terminal .. " -e " .. editor

browser    = "qutebrowser"
browser2   = "inox"
gui_editor = "emacs"
graphics   = "gimp"
musicplr   = terminal .. " -e ncmpcpp "

modkey = "Mod4"
altkey = "Mod1"


local function client_menu_toggle ()
   local instance = nil

   return function ()
      if instance and instance.wibox.visible then
         instance:hide()
         instance = nil
      else
         instance = awful.menu.clients({ theme = { width = 250 } })
      end
   end
end

local function client_set_border(c)
   if c.fullscreen then
      c.border_width = 0
   elseif c.border_width == 0 and not c.no_border then
      c.border_width = beautiful.border_width
   end
end

local function client_resize (key, c)
   if c == nil then
      c = client.focus
   end


   if c.floating then
      if     key == "Up"    then c:relative_move(0, 0, 0, -5)
      elseif key == "Down"  then c:relative_move(0, 0, 0, 5)
      elseif key == "Right" then c:relative_move(0, 0, 5, 0)
      elseif key == "Left"  then c:relative_move(0, 0, -5, 0)
      elseif key == "Next"  then c:relative_move( 20,  20, -40, -40)
      elseif key == "Prior" then c:relative_move(-20, -20,  40,  40)
      else
         return false
      end
   else
      if     key == "Up"    then awful.client.incwfact(-0.05)
      elseif key == "Down"  then awful.client.incwfact(0.05)
      elseif key == "Right" then awful.tag.incmwfact(0.05)
      elseif key == "Left"  then awful.tag.incmwfact(-0.05)
      else
         return false
      end
   end

   return true
end

local function tag_view_nonempty (direction, sc)
   local s = sc or awful.screen.focused()

   for i = 1, #s.tags do
      awful.tag.viewidx(direction, s)
      if #s.clients > 0 then
         return
      end
   end
end

local function notify_callback (args)
   local should_suspend = false
   if args.freedesktop_hints ~= nil and args.freedesktop_hints.urgency == "\2" then
      args.ignore_suspend = true
   end

   for _, c in pairs(awful.screen.object.get_clients()) do
      if c.fullscreen then
         if not naughty.is_suspended() then
            naughty.suspend()
         end
         return
      end
   end
   
   if naughty.is_suspended() and not notify_suspended then
      naughty.resume()
   end

   return args
end

local function system_lock ()
   awful.spawn("loginctl lock-session")
end

local function system_suspend ()
   awful.spawn("systemctl suspend")
end

local function system_hibernate ()
   awful.prompt.run {
      prompt       = "Hibernate (type 'yes' to confirm)? ",
      textbox      = awful.screen.focused().mypromptbox.widget,
      exe_callback = function (t)
         if string.lower(t) == "yes" then
            awful.spawn("systemctl hibernate")
         end
      end,
      completion_callback = function (t, p, n)
         return awful.completion.generic(t, p, n, {"no", "NO", "yes", "YES"})
      end
   }
end

local function system_hybrid_sleep ()
   awful.prompt.run {
      prompt       = "Hybrid Sleep (type 'yes' to confirm)? ",
      textbox      = awful.screen.focused().mypromptbox.widget,
      exe_callback = function (t)
         if string.lower(t) == "yes" then
            awful.spawn("systemctl hybrid-sleep")
         end
      end,
      completion_callback = function (t, p, n)
         return awful.completion.generic(t, p, n, {"no", "NO", "yes", "YES"})
      end
   }
end

local function system_reboot ()
   awful.prompt.run {
      prompt       = "Reboot (type 'yes' to confirm)? ",
      textbox      = awful.screen.focused().mypromptbox.widget,
      exe_callback = function (t)
         if string.lower(t) == "yes" then
            awesome.emit_signal("exit", nil)
            awful.spawn("systemctl reboot")
         end
      end,
      completion_callback = function (t, p, n)
         return awful.completion.generic(t, p, n, {"no", "NO", "yes", "YES"})
      end
   }
end

local function system_power_off ()
   awful.prompt.run {
      prompt       = "Power Off (type 'yes' to confirm)? ",
      textbox      = awful.screen.focused().mypromptbox.widget,
      exe_callback = function (t)
         if string.lower(t) == "yes" then
            awesome.emit_signal("exit", nil)
            awful.spawn("systemctl poweroff")
         end
      end,
      completion_callback = function (t, p, n)
         return awful.completion.generic(t, p, n, {"no", "NO", "yes", "YES"})
      end
   }
end

local function take_screenshot (opts)
   if opts == nil then
      opts = ""
   end
   local sound = "/usr/share/sounds/freedesktop/stereo/screen-capture.oga"

   awful.spawn.easy_async_with_shell("maim ~/Pictures/screenshot-$(date +%Y-%m-%d_%H-%M-%S).png " .. opts,
                                     function ()
                                        awful.spawn.with_shell("paplay " .. sound)
   end)
end

local function set_wallpaper(s, wallpaper)
   local wallpaper = wallpaper or beautiful.wallpaper
   if wallpaper then
      if type(wallpaper) == "function" then
         wallpaper = wallpaper(s)
      end
      gears.wallpaper.maximized(wallpaper, s, true)
   end
end

local function load_wallpaper (wallpaper_path)
   local wallpaper = config_path .. "theme/_wall.jpg"
   -- TODO: replace io.open with a non-blocking
   -- https://awesomewm.org/doc/api/libraries/awful.spawn.html
   local ln = io.popen("ln -sfn '" .. wallpaper_path .. "' '" .. config_path .. "theme/_wall.jpg'")
   ln:close()
   for s = 1, screen.count() do
      set_wallpaper(s, wallpaper)
   end
end

local function get_wallpaper_menu (wallpaper_path, is_submenu)
   local wallpaper_path = wallpaper_path or home_path .. "Pictures/Wallpapers"
   local wallmenu = {}

   if not is_submenu then
      local default_item = {
         "Default",
         function ()
            awful.spawn.with_shell("rm " .. config_path .. "theme/_wall.jpg")
            for s = 1, screen.count() do
               set_wallpaper(s)
            end
         end
      }
      table.insert(wallmenu, default_item)
   end
   local files = io.popen("ls -1 --group-directories-first '" .. wallpaper_path .. "'")
   for line in files:lines() do      
      local line_len = string.len(line) * 10
      
      if string.match(line, "%.png$") or string.match(line ,"%.jp[e]?g$") then
         local wallpaper_name = line
         local wallpaper = wallpaper_path .. "/" .. wallpaper_name
         local item = { wallpaper_name, function () load_wallpaper(wallpaper) end, theme={ width=line_len } }
         table.insert(wallmenu, item)
      else
         local dir_name = line
         local sub_path = wallpaper_path .. "/" .. dir_name
         local check = io.open(sub_path, "r")
         if check ~= nil then
            local ok, err, code = check:read(1)
            check:close()
            if code == 21 then
               local sub_items = get_wallpaper_menu(sub_path, true)
               if next(sub_items) ~= nil then
                  table.insert(wallmenu, { dir_name, sub_items, theme={ width=line_len } })
               end
            end
         end
      end
   end
   files:close()
   return wallmenu
end
naughty.config.notify_callback = notify_callback

cyclefocus.show_clients = true
cyclefocus.focus_clients = true
cyclefocus.display_prev_count = 1
cyclefocus.default_preset = {
   position = "top_left", -- deprecated options
   timeout = 0,
   margin = 0,
   border_width = 0,
   border_color = "#001E21",
   fg = "#00ffff",
   bg = "#001214"
}

awful.layout.layouts = {
   awful.layout.suit.tile,
   awful.layout.suit.tile.left,
   awful.layout.suit.tile.bottom,
   awful.layout.suit.tile.top,
   awful.layout.suit.fair,
   awful.layout.suit.fair.horizontal,
   awful.layout.suit.spiral,
   awful.layout.suit.spiral.dwindle,
   awful.layout.suit.max,
   awful.layout.suit.max.fullscreen,
   awful.layout.suit.magnifier,
   awful.layout.suit.corner.nw,
   awful.layout.suit.corner.ne,
   awful.layout.suit.corner.sw,
   awful.layout.suit.corner.se,
   awful.layout.suit.floating,
}
myawesomemenu = {
   { "&hotkeys", function() return false, hotkeys_popup.show_help end},
   { "&manual", terminal .. " -e man awesome" },
   { "&edit config", editor_cmd .. " " .. awesome.conffile },
   { "&restart", awesome.restart },
   { "&quit", function() awesome.quit() end}
}

mysystemmenu = {
   --{ "manual", tools.terminal .. " -e man awesome" },
   { "&lock", system_lock },
   { "&suspend", system_suspend },
   { "hi&bernate", system_hibernate },
   { "hybri&d sleep", system_hybrid_sleep },
   { "&reboot", system_reboot },
   { "&power off", system_power_off }
}

mywallpapermenu = get_wallpaper_menu()

mymainmenu = awful.menu({
      items = {
         { "&Awesome", myawesomemenu, beautiful.menu_icon },
         { "&System", mysystemmenu },
         { "&Wallpapers", mywallpapermenu },
         { "&Terminal", terminal }
      }
})

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

app_folders =  { "~/.local/share/applications", "/usr/share/applications/", "/usr/local/share/applications" }
menubar.menu_gen.all_menu_dirs = app_folders
menubar.utils.terminal = terminal -- Set the terminal for applications that require it

mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
local mytextclock = wibox.widget.textclock()
local calendar = awful.widget.calendar_popup.month()
calendar:attach(mytextclock, "tr")

-- Create a wibox for each screen and add it
local taglist_buttons = awful.util.table.join(
   awful.button({ }, 1, function(t) t:view_only() end),
   awful.button({ modkey }, 1, function(t)
         if client.focus then
            client.focus:move_to_tag(t)
         end
   end),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, function(t)
         if client.focus then
            client.focus:toggle_tag(t)
         end
   end),
   awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
   awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = awful.util.table.join(
   awful.button({ }, 1, function (c)
         if c == client.focus then
            c.minimized = true
         else
            -- Without this, the following
            -- :isvisible() makes no sense
            c.minimized = false
            if not c:isvisible() and c.first_tag then
               c.first_tag:view_only()
            end
            -- This will also un-minimize
            -- the client, if needed
            client.focus = c
            c:raise()
         end
   end),
   awful.button({ }, 3, client_menu_toggle()),
   awful.button({ }, 4, function ()
         awful.client.focus.byidx(1)
   end),
   awful.button({ }, 5, function ()
         awful.client.focus.byidx(-1)
end))

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", function(s) set_wallpaper(s) end)

awful.screen.connect_for_each_screen(function(s)
      -- Widgets separators
      local separator1px = wibox.widget.imagebox()
      separator1px:set_image(beautiful.get().spr1px)
      local separator2px = wibox.widget.imagebox()
      separator2px:set_image(beautiful.get().spr2px)
      local separator4px = wibox.widget.imagebox()
      separator4px:set_image(beautiful.get().spr4px)
      local separator5px = wibox.widget.imagebox()
      separator5px:set_image(beautiful.get().spr5px)
      local separator10px = wibox.widget.imagebox()
      separator10px:set_image(beautiful.get().spr10px)

      -- Wallpaper
      set_wallpaper(s)

      -- Each screen has its own tag table.
      layouts = awful.layout.layouts
      tags = {
         names = { "1", "2", "3", "4", "5", "6" },
         layouts = { layouts[1], layouts[2], layouts[10], layouts[10], layouts[1], layouts[12] }
      }
      awful.tag(tags.names, s, tags.layouts)

      -- Create a promptbox for each screen
      s.mypromptbox = awful.widget.prompt()
      -- Create an imagebox widget which will contains an icon indicating which layout we're using.
      -- We need one layoutbox per screen.
      s.mylayoutbox = awful.widget.layoutbox(s)
      s.mylayoutbox:buttons(awful.util.table.join(
                               awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                               awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                               awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                               awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
      -- Create a taglist widget
      s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

      -- Create a tasklist widget
      s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)

      -- Create a systray widget
      local mysystray = wibox.widget.systray()
      local mysystraymargin = wibox.container.margin(mysystray, 0, 0, 2, 2)

      -- Create the wibox
      s.mywibox = awful.wibox({ position = "top", screen = s })

      -- Widgets that are aligned to the left
      local left_layout = wibox.layout.fixed.horizontal()
      left_layout:add(s.mytaglist)
      left_layout:add(s.mypromptbox)


      -- Widgets that are aligned to the right
      local right_layout = wibox.layout.fixed.horizontal()
      right_layout:add(s.mylayoutbox)

      -- Add widgets to the wibox
      s.mywibox:setup {
         layout = wibox.layout.align.horizontal,
         left_layout,  -- Left widget
         s.mytasklist, -- Middle widget
	 right_layout, -- Right widget
      }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
                awful.button({ }, 3, function () mymainmenu:toggle() end),
                awful.button({ }, 4, awful.tag.viewnext),
                awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}


-- {{{ Key bindings
globalkeys = gears.table.join(
   awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
      {description="show help", group="awesome"}),
   awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
      {description = "view previous", group = "tag"}),
   awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
      {description = "view next", group = "tag"}),
   awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
      {description = "go back", group = "tag"}),

   -- Non-empty tag browsing
   awful.key({ modkey,            }, "Next",
      function ()
         tag_view_nonempty(1)
      end, {description = "focus next non-empty tag", group = "tag"}),
   awful.key({ modkey,            }, "Prior",
      function ()
         tag_view_nonempty(-1)
      end, {description = "focus previous non-empty tag", group = "tag"}),

   -- Take a screenshot
   awful.key({                   }, "Print", take_screenshot,
      {description = "print a screenshot", group = "screenshot"}),
   awful.key({ altkey            }, "Print", function () take_screenshot("--delay=10") end,
      {description = "print a screenshot after 10 sec", group = "screenshot"}),
   awful.key({ modkey            }, "Print", function () take_screenshot("-s") end,
      {description = "print a screenshot by area", group = "screenshot"}),

   -- Default client focus
   awful.key({ modkey,           }, "j",
      function ()
         awful.client.focus.byidx( 1)
         if client.focus then client.focus:raise() end
      end, {description = "focus next by index", group = "client"}),
   awful.key({ modkey,           }, "k",
      function ()
         awful.client.focus.byidx(-1)
         if client.focus then client.focus:raise() end
      end, {description = "focus previous by index", group = "client"}),

   -- Layout manipulation
   awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(1)      end,
      {description = "swap with next client by index", group = "client"}),
   awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx(-1)     end,
      {description = "swap with previous client by index", group = "client"}),
   awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative(1)  end,
      {description = "focus the next screen", group = "screen"}),
   awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
      {description = "focus the previous screen", group = "screen"}),
   awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
      {description = "jump to urgent client", group = "client"}),
   awful.key({ modkey,           }, "Tab",
      function ()
         awful.client.focus.history.previous()
         if client.focus then
            client.focus:raise()
         end
      end, {description = "go back", group = "client"}),
   awful.key({ altkey,          }, "Tab",
      function(c)
         cyclefocus.cycle(1)
      end, {description = "cycle focus next client", group = "client"}),
   awful.key({ altkey, "Shift"  }, "Tab",
      function(c)
         cyclefocus.cycle(-1)
      end, {description = "cycle focus previous client", group = "client"}),

   awful.key({ modkey,           }, "l",     function () client_resize('Right')         end,
      {description = "increase master width factor", group = "layout"}),
   awful.key({ modkey,           }, "h",     function () client_resize('Left')        end,
      {description = "decrease master width factor", group = "layout"}),
   awful.key({ modkey, altkey    }, "l",     function () client_resize('Up')           end,
      {description = "increase client height factor", group = "layout"}),
   awful.key({ modkey, altkey    }, "h",     function () client_resize('Down')         end,
      {description = "decrease client height factor", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end,
      {description = "increase the number of master clients", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end,
      {description = "decrease the number of master clients", group = "layout"}),
   awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end,
      {description = "increase the number of columns", group = "layout"}),
   awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end,
      {description = "decrease the number of columns", group = "layout"}),
   awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end,
      {description = "select next layout", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end,
      {description = "select previous layout", group = "layout"}),
   awful.key({ modkey, "Control" }, "t",
      function ()
         awful.layout.set(awful.layout.suit.tile)
      end, {description = "switch to tile layout", group = "layout"}),
   awful.key({ modkey, "Control" }, "m",
      function ()
         awful.layout.set(awful.layout.suit.max)
      end, {description = "switch to max layout", group = "layout"}),
   awful.key({ modkey, "Control" }, "f",
      function ()
         awful.layout.set(awful.layout.suit.floating)
      end, {description = "switch to floating layout", group = "layout"}),

   awful.key({ modkey, "Control" }, "n",
      function ()
         local c = awful.client.restore()
         -- Focus restored client
         if c then
            client.focus = c
            c:raise()
         end
      end, {description = "restore minimized", group = "client"}),

   -- Standard program
   awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
      {description = "open a terminal", group = "launcher"}),
   awful.key({ modkey, "Control" }, "r", awesome.restart,
      {description = "reload awesome", group = "awesome"}),
   awful.key({ modkey, "Shift"   }, "q", awesome.quit,
      {description = "quit awesome", group = "awesome"}),

   -- System volume
   awful.key({                   }, "XF86AudioRaiseVolume", myvolume.raise),
   awful.key({                   }, "XF86AudioLowerVolume", myvolume.lower),
   awful.key({                   }, "XF86AudioMute", myvolume.mute),

   -- System brightness
   awful.key({                   }, "XF86MonBrightnessDown", function() mybrightness:down() end),
   awful.key({                   }, "XF86MonBrightnessUp", function() mybrightness:up() end),

   -- Toggle sloppy focus
   awful.key({ modkey, "Shift"   }, "s",
      function ()
         sloppy_focus = not sloppy_focus
      end, {description = "toggle sloppy focus", group = "client"}),

   -- Notifications
   awful.key({ modkey, "Shift"   }, "d",
      function ()
         -- TODO: move to external widget with icon
         if naughty.is_suspended() then
            notify_suspended = false
            naughty.resume()
         else
            notify_suspended = true
            naughty.suspend()
         end
      end, {description = "enabled/disable notifications", group = "client"}),

   -- System
   awful.key({ modkey,           }, "Home", system_lock,
      {description = "lock the screen", group = "system"}),
   awful.key({ modkey,           }, "End", system_suspend,
      {description = "suspend the system", group = "system"}),
   awful.key({                   }, "XF86Sleep", system_suspend),
   awful.key({ modkey, "Shift"   }, "Home", system_hibernate,
      {description = "hibernate the system", group = "system"}),
   awful.key({ modkey            }, "XF86Sleep", system_hibernate),
   awful.key({ modkey, "Shift"   }, "End", system_hybrid_sleep,
      {description = "send to hybrid sleep", group = "system"}),
   awful.key({ modkey,           }, "Insert", system_reboot,
      {description = "reboot the system", group = "system"}),
   awful.key({ modkey,           }, "Delete", system_power_off,
      {description = "shutdown the system", group = "system"}),


   -- Prompt
   awful.key({ modkey },            "r",
      function ()
         awful.prompt.run {
            prompt              = "Run: ",
            textbox             = awful.screen.focused().mypromptbox.widget,
            completion_callback = awful.completion.shell,
            history_path        = awful.util.get_cache_dir() .. "/history"
         }
      end,
      {description = "run prompt", group = "launcher"}),

   awful.key({ modkey,           }, "x",
      function ()
         awful.prompt.run {
            prompt       = "Run Lua code: ",
            textbox      = awful.screen.focused().mypromptbox.widget,
            exe_callback = awful.util.eval,
            history_path = awful.util.get_cache_dir() .. "/history_eval"
         }
      end,
      {description = "lua execute prompt", group = "awesome"}),

   -- Menubar
   awful.key({ altkey         }, "Escape",
      function ()
         -- If you want to always position the menu on the same place set coordinates
         awful.menu.menu_keys.down = { "Down", "Alt_L" }
         awful.menu.clients({theme = { width = 250 }}, { keygrabber=true, coords={x=525, y=330} })
      end, {description = "show app switcher", group = "awesome"}),
   awful.key({ modkey,         }, "a", function () awful.spawn("rofi -show", false) end,
      {description = "show rofi", group = "launcher"}),
   awful.key({ modkey,         }, "p", function() menubar.show() end,
      {description = "show the menubar", group = "launcher"}),
   awful.key({ modkey,         }, "w", function () mymainmenu:show() end,
      {description = "show main menu", group = "awesome"}),
   awful.key({ modkey, "Shift" }, "w",
      function ()
         mymainmenu:show({ keygrabber = true })
      end,  {description = "show main menu with keygrabber", group = "awesome"})
)

clientkeys = gears.table.join(
   awful.key({ modkey,           }, "f",
      function (c)
         c.fullscreen = not c.fullscreen
         c:raise()
      end, {description = "toggle fullscreen", group = "client"}),
   awful.key({ altkey,           }, "F2",
      function (c)
         if c.pid then
            awful.spawn("kill -9 " .. c.pid)
         else
            awful.spawn("xkill")
         end
      end,
      {description = "kill", group = "client"}),
   awful.key({ altkey,           }, "F4",     function (c) c:kill()                         end,
      {description = "close", group = "client"}),
   awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
      {description = "close", group = "client"}),
   awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle,
      {description = "toggle floating", group = "client"}),
   awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
      {description = "move to master", group = "client"}),
   awful.key({ modkey,  "Shift"  }, "Left",   function (c) awful.client.setmaster(c)        end,
      {description = "set to master", group = "client"}),
   awful.key({ modkey,  "Shift"  }, "Right",  function (c) awful.client.setslave(c)         end,
      {description = "move to slave", group = "client"}),
   awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
      {description = "move to screen", group = "client"}),
   awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
      {description = "toggle keep on top", group = "client"}),
   awful.key({ modkey,           }, "y",      function (c) c.sticky = not c.sticky          end,
      {description = "toggle keep sticky", group = "client"}),
   awful.key({ modkey, "Shift"   }, "t",
      function (c)
         awful.titlebar.toggle(c)
      end,
      {description = "toggle title bar", group = "client"}),
   awful.key({ modkey,           }, "n",
      function (c)
         -- The client currently has the input focus, so it cannot be
         -- minimized, since minimized clients can't have the focus.
         c.minimized = true
      end, {description = "minimize", group = "client"}),
   awful.key({ modkey,           }, "m",
      function (c)
         c.maximized = not c.maximized
         c:raise()
      end, {description = "(un)maximize", group = "client"}),
   awful.key({ modkey, altkey   }, "m",
      function (c)
         c.maximized_horizontal = not c.maximized_horizontal
         c.maximized_vertical   = not c.maximized_vertical
         c:raise()
      end, {description = "(un)maximize horizontally/vertically", group = "client"}),
   awful.key({ modkey, "Shift"   }, "m",
      function (c)
         c.maximized_horizontal = not c.maximized_horizontal
         c:raise()
      end, {description = "(un)maximize horizontally", group = "client"}),
   awful.key({ modkey, "Control" }, "m",
      function (c)
         c.maximized_vertical = not c.maximized_vertical
         c:raise()
      end, {description = "(un)maximize vertically", group = "client"}),
   -- Snap
   awful.key({ modkey,           }, "Up",
      function (c)
         c.maximized = true
         c:raise()
      end, {description = "maximize", group = "client"}),
   awful.key({ modkey,           }, "Down",
      function (c)
         c.maximized = false
         c:raise()
         awful.placement.centered(c,nil)
      end, {description = "unmaximize", group = "client"}),
   -- Resize
   awful.key({ modkey, altkey    }, "s",
      function (c)
         keygrabber.run(function(mod, key, event)
               if event == "release" then return end

               local makeResize = client_resize(key, c)

               if makeResize == false then
                  keygrabber.stop()
               end
         end)
      end, {description = "resize the client", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
   globalkeys = gears.table.join(
      globalkeys,
      -- View tag only.
      awful.key({ modkey }, "#" .. i + 9,
         function ()
            local screen = awful.screen.focused()
            local tag = screen.tags[i]
            if tag then
               tag:view_only()
            end
         end,
         {description = "view tag #"..i, group = "tag"}),
      -- Toggle tag display.
      awful.key({ modkey, "Control" }, "#" .. i + 9,
         function ()
            local screen = awful.screen.focused()
            local tag = screen.tags[i]
            if tag then
               awful.tag.viewtoggle(tag)
            end
         end,
         {description = "toggle tag #" .. i, group = "tag"}),
      -- Move client to tag.
      awful.key({ modkey, "Shift" }, "#" .. i + 9,
         function ()
            if client.focus then
               local tag = client.focus.screen.tags[i]
               if tag then
                  client.focus:move_to_tag(tag)
               end
            end
         end,
         {description = "move focused client to tag #"..i, group = "tag"}),
      -- Toggle tag on focused client.
      awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
         function ()
            if client.focus then
               local tag = client.focus.screen.tags[i]
               if tag then
                  client.focus:toggle_tag(tag)
               end
            end
         end,
         {description = "toggle focused client on tag #" .. i, group = "tag"})
   )
end

clientbuttons = awful.util.table.join(
   awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
   awful.button({ modkey }, 1, awful.mouse.client.move),
   awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

function titlebar_add(c)
   -- buttons for the titlebar
   local buttons = gears.table.join(
      awful.button({ }, 1, function()
            client.focus = c
            c:raise()
            awful.mouse.client.move(c)
      end),
      awful.button({ }, 3, function()
            client.focus = c
            c:raise()
            awful.mouse.client.resize(c)
      end)
   )

   awful.titlebar(c, {size = 16}) : setup
   {
      { -- Left
         -- awful.titlebar.widget.iconwidget(c),
         buttons = buttons,
         layout  = wibox.layout.fixed.horizontal
      },
      { -- Middle
         { -- Title
            align  = "center",
            widget = awful.titlebar.widget.titlewidget(c)
         },
         buttons = buttons,
         layout  = wibox.layout.flex.horizontal
      },
      { -- Right
         awful.titlebar.widget.floatingbutton (c),
         awful.titlebar.widget.maximizedbutton(c),
         awful.titlebar.widget.stickybutton   (c),
         awful.titlebar.widget.ontopbutton    (c),
         awful.titlebar.widget.closebutton    (c),
         layout = wibox.layout.fixed.horizontal()
      },
      layout = wibox.layout.align.horizontal
   }
end

function awful.rules.extra_properties.icon (c, value, props)
   awful.spawn.with_shell("xseticon -id " .. c.window .. " " .. value)
end

function awful.rules.extra_properties.titlebars_show (c, value, props)
   -- Custom property to hide/show titlebars
   -- titlebar.toggle doesn't work fine if 'titlebars_enabled' is false
   if value then
      awful.titlebar.show(c)
   else
      awful.titlebar.hide(c)
   end
end

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
   -- All clients will match this rule.
   { rule = { },
     properties = {
        border_width = beautiful.border_width,
        border_color = beautiful.border_normal,
        focus = awful.client.focus.filter,
        raise = true,
        keys = clientkeys,
        buttons = clientbuttons,
        screen = awful.screen.preferred,
        titlebars_enabled = true,
        titlebars_show = false,
        placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
   },
   -- Floating clients.
   { rule_any = {
        instance = {
           "DTA",  -- Firefox addon DownThemAll.
           "copyq",  -- Includes session name in class.
        },
        class = {
           "Arandr",
           "Gpick",
           "Kruler",
           "MessageWin",  -- kalarm.
           "Sxiv",
           "Wpa_gui",
           "pinentry",
           "veromix",
           "xtightvncviewer"},

        name = {
           "Event Tester",  -- xev.
        },
        role = {
           "AlarmWindow",  -- Thunderbird's calendar.
           "pop-up",       -- e.g. Developer Tools.
           "About"
        }
   }, properties = { floating = true }},

   -- Add titlebars to normal clients and dialogs
   { rule_any = { type = { "dialog" } },
     except_any = { role = { "notify_dialog" }},
     properties = {
        titlebars_show = true,
        floating = true,
        screen = awful.screen.focused,
        placement = awful.placement.under_mouse+awful.placement.no_overlap+awful.placement.no_offscreen,
        callback = awful.client.setslave
     }
   },
   -- Custom
   { rule_any = { class = { "MPlayer", "mpv" } },
     properties = { floating = true }
   },
   { rule = { instance = "mps" },
     properties = {
        screen = 2,
        skip_taskbar = true,
        floating = true,
        ontop = true,
        raise = false,
        focus = false,
        width = 300,
        x = 12,
        y = 25
     }
   },
   { rule = { class = "pinentry" },
     properties = { floating = true }
   },
   { rule = { class = "Gimp.*" },
     properties = { tag = "6", floating = true }
   },
   { rule_any = { class = { "URxvt", ".*ermina.*" } },
     properties = { tag = "2", size_hints_honor = false, icon = "/usr/share/icons/Moka/48x48/apps/terminal.png" }
   },
   { rule = { class = "Emacs" },
     properties = { tag = "3", switch_to_tags = true, size_hints_honor = false }
   },
   { rule_any = { instance = { "Ranger" }, name = { ".*ranger:.*" } },
     properties = { tag = "6", screen = 2, size_hints_honor = false, icon = "/usr/share/icons/Moka/48x48/apps/file-manager.png" }
   },
   { rule_any = { instance = { "Mc" }, name = { ".*mc .*" } },
     properties = { tag = "6", screen = 1, switch_to_tags = true, size_hints_honor = false, icon = "/usr/share/icons/Moka/48x48/apps/file-manager.png" }
   },
   { rule_any = { role = { "browser" }, class = { "Epiphany" }},
     properties = { tag = "4", maximized_vertical = true, maximized_horizontal = true }
   },
   { rule_any = { instance = { "WeeChat" }, name = { ".*weeChat.*" } },
     properties = {
        tag = "5", switch_to_tags = true, maximized_vertical = true, maximized_horizontal = true, icon = "/usr/share/icons/hicolor/32x32/apps/weechat.png"
     }
   },
   { rule_any = { name = {"^Android Emulator*", "^Emulator"} },
     properties = {
        floating = true,
        -- skip_taskbar = true, 
        callback = function(c)
           -- force due the behavior in property::size
           c.border_width = 0
           c.no_border = true
        end
     }
   },
   { rule = { name = "^Emulator", type = "utility"},
     properties = {
        skip_taskbar = true,
        focusable = false
     }
   },
   { rule = { instance = "Pidgin" },
     properties = { tag = "5", size_hints_honor = false, floating = true }
   },
   { rule = { class = "Pidgin", role = "conversation" },
     properties = { width = 1000, height = 670, x = 320, y = 55 }},
   -- {rule = {class = "Pidgin", role = "accounts"},
   --  properties = {width = 500, height = 500, x = 0, y = 0}},
   { rule = { class = "Pidgin", role = "buddy_list" },
     properties = { width = 300, height = 670, x = 10, y = 55 },
     callback = awful.client.setslave
   },
   { rule = { class = "Kodi" },
     properties = { tag = "1", screen = 2, fullscreen = true, ontop = true, switch_to_tags = true }
   }
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal(
   "manage", function (c)
      -- Set the windows at the slave,
      -- i.e. put it at the end of others instead of setting it master.
      -- if not awesome.startup then awful.client.setslave(c) end

      if awesome.startup and
         not c.size_hints.user_position
      and not c.size_hints.program_position then
         -- Prevent clients from being unreachable after screen count changes.
         awful.placement.no_offscreen(c)
      end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", titlebar_add)


-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal(
   "mouse::enter", function(c)
      if sloppy_focus and awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
      and awful.client.focus.filter(c) then
         client.focus = c
      end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
client.connect_signal("property::size", client_set_border)
client.connect_signal("property::fullscreen", client_set_border)
-- }}}

-- {{{ Autorun apps
awful.spawn.with_shell(config_path .. "autorun.sh", false)
-- }}}
