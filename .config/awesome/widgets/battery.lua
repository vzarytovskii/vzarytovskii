--[[

   Awesome WM Battery Widget
   Distopico Vegan <distopico [at] riseup [dot] net>
   Licensed under GPL3

   Original from: https://github.com/mrzapp/awesomerc

--]]

local wibox = require("wibox")
local awful = require("awful")
local naughty = require("naughty")
local beautiful = require("beautiful")
local brightness = require("widgets/brightness")
local helpers = require("widgets/helpers")

local config = awful.util.getdir("config")
local widget = {}
local popup = nil
local batterytext = "--"
local iconpath = ""

-- {{{ Define adapter
local adapter = "BAT1"
local acAdapter = "ACAD"
local charge = "charge"


-- {{{ Define subwidgets
widget.text = wibox.widget.textbox()
widget._icon = wibox.widget.imagebox()

-- Change the draw method so icons can be drawn smaller
-- helpers:set_draw_method(widget._icon)
-- }}}

-- {{{ Define interactive behaviour
widget._icon:buttons(awful.util.table.join(
                        awful.button({ }, 1, function () awful.util.spawn("gnome-control-center power") end)
))
-- }}}

-- {{{ Check adapter method
function widget:check()
   local adapters = string.gmatch(helpers:run("ls /sys/class/power_supply/"), "%S+")
   for value in adapters do
      if value:match("^A") then
         acAdapter = value
      elseif value:match("^B") then
         adapter = value
      end
   end

   -- Test identifier
   charge = "charge"
   widget.hasbattery = helpers:test("cat /sys/class/power_supply/" .. adapter .. "/" .. charge .. "_now")

   -- Try another identifier
   if not widget.hasbattery then
      charge = "energy"
      widget.hasbattery = helpers:test("cat /sys/class/power_supply/" .. adapter .. "/" .. charge .. "_now")
   end
end
-- }}}

-- {{{ Update method
function widget:update()
   local sendNotify = false
   local cur = helpers:run("cat /sys/class/power_supply/" ..adapter .. "/" .. charge .. "_now")
   local cap = helpers:run("cat /sys/class/power_supply/" ..adapter .. "/" .. charge .. "_full")
   local sta = helpers:run("cat /sys/class/power_supply/" ..adapter .. "/status")
   local ac = helpers:run("cat /sys/class/power_supply/" ..acAdapter .. "/online")

   if cur and cap then
      local acStatus = ac ~= "" and math.floor(ac) or 0;
      local battery = math.floor(cur * 100 / cap)
      local colorfg = beautiful.fg_urgent
      local toHibernate = false
      if acStatus == 1 then
         batterytext = battery .. "% Battery | AC"
      else
         batterytext = battery .. "% Battery"
      end
      iconpath = config.."/theme/icons/status/battery"

      if(battery < 5) then
         iconpath = iconpath .. "-caution"
         colorfg = "#FF2B2B"
         toHibernate = true

      elseif(battery < 10) then
         iconpath = iconpath .. "-caution"
         colorfg = "#FF5757"

      elseif(battery < 25) then
         iconpath = iconpath .. "-low"

      elseif(battery < 75) then
         iconpath = iconpath .. "-good"

      else
         iconpath = iconpath .. "-full"

      end

      if sta:match("Charging") then
         iconpath = iconpath .. "-charging"
      end

      iconpath = iconpath .. "-symbolic.svg"

      widget._icon:set_image(iconpath)
      widget.icon = helpers:set_draw_method(widget._icon)

      widget.text:set_markup(batterytext)

      if ((battery == 18 or battery == 10 or battery < 5) and sta:match("Discharging")) then
         sendNotify = true
         batterytext = batterytext .. "  Warning low level batery!"
         brightness:set(40)

         if(toHibernate) then
            brightness:set(30)
            batterytext = batterytext .. "  - Prepare Hibernate"
            helpers:delay(function() awful.util.spawn("systemctl hibernate") end, 10)
         end
      end

      if (sendNotify) then
         naughty.notify({
               icon = iconpath,
               icon_size = 30,
               text = batterytext,
               timeout = 4, hover_timeout = 0.5,
               screen = mouse.screen,
               fg = colorfg,
               ignore_suspend = true
         })
      end

   else
      widget.text:set_markup("N/A")

   end
end

function widget:show()
   popup = naughty.notify({
         icon = iconpath,
         icon_size = 16,
         text = batterytext,
         timeout = 0, hover_timeout = 0.5,
         screen = mouse.screen,
         ignore_suspend = true
   })
end

function widget:hide()
   if popup ~= nil then
      naughty.destroy(popup)
      popup = nil
   end
end
-- }}}

-- {{{ Listen if battery was found
widget:check()

if widget.hasbattery then
   helpers:listen(widget)

   widget._icon:connect_signal("mouse::enter", function() widget:show() end)
   widget._icon:connect_signal("mouse::leave", function() widget:hide() end)
end
-- }}}

return widget
