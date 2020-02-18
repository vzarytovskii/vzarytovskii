--[[

   Awesome WM Volumen Widget
   Distopico Vegan <distopico [at] riseup [dot] net>
   Licensed under GPL3

   Original from: https://github.com/mrzapp/awesomerc

--]]

local wibox = require("wibox")
local awful = require("awful")
local naughty = require("naughty")
local helpers = require("widgets/helpers")

local config = awful.util.getdir("config")
local widget = {}
local popup = nil
local volumetext = "--"
local volumecurrent = nil
local iconpath = ""

-- {{{ Define subwidgets
widget.text = wibox.widget.textbox()
widget._icon = wibox.widget.imagebox()

-- Change the draw method so icons can be drawn smaller
-- helpers:set_draw_method(widget.icon)
-- }}}

-- {{{ Define interactive behaviour
widget._icon:buttons(awful.util.table.join(
                        awful.button({ }, 1, function () awful.util.spawn("gnome-control-center sound") end)
))
-- }}}

-- {{{ Update method
function widget:update()
   -- TODO: change amixer to pactl
   -- see: https://unix.stackexchange.com/questions/132230/read-out-pulseaudio-volume-from-commandline-i-want-pactl-get-sink-volume
   -- https://gist.github.com/Ropid/3a08d70a807e35c7e8c688d54b725e8e
   local status = helpers:run("amixer sget Master")
   local volume = tonumber(string.match(status, "(%d?%d?%d)%%")) or 0
   volumetext = volume .. "% Volume"

   widget.text:set_markup(volumetext)

   iconpath = config.."/theme/icons/status/audio-volume"

   if string.find(status, "[off]", 1, true) or volume <= 0.0 then
      iconpath = iconpath .. "-muted"

   elseif volume < 25 then
      iconpath = iconpath .. "-low"

   elseif volume > 75 then
      iconpath = iconpath .. "-high"

   else
      iconpath = iconpath .. "-medium"

   end

   iconpath = iconpath .. "-symbolic.svg"

   if volumecurrent ~= volume and volumecurrent ~= nil then
      widget.showPopup(0.1)
   end
   widget._icon:set_image(iconpath)
   widget.icon = helpers:set_draw_method(widget._icon)

   volumecurrent = volume

end


function widget:showPopup(timeout)
   widget.hidePopup()
   popup = naughty.notify({
         icon = iconpath,
         icon_size = 16,
         text = volumetext,
         timeout = timeout, hover_timeout = 0.5,
         screen = mouse.screen,
         ignore_suspend = true
   })
end

function widget:hidePopup()
   if popup ~= nil then
      naughty.destroy(popup)
      popup = nil
   end
end

function widget:raise()
   awful.util.spawn("amixer set Master 9%+", false)

   helpers:delay(widget.update, 0.1)
end

function widget:lower()
   awful.util.spawn("amixer set Master 9%-", false)

   helpers:delay(widget.update, 0.1)
end

function widget:mute()
   awful.util.spawn("amixer -D pulse set Master 1+ toggle", false)

   helpers:delay(widget.update, 0.1)
end

-- }}}

-- {{{ Listen
helpers:listen(widget, 40)

widget._icon:connect_signal("mouse::enter", function() widget:showPopup() end)
widget._icon:connect_signal("mouse::leave", function() widget:hidePopup() end)
-- }}}

return widget
