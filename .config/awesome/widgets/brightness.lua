--[[

   Awesome WM Brightness Widget
   Distopico Vegan <distopico [at] riseup [dot] net>
   Licensed under GPL3

   @author Tyler Compton <https://github.com/velovix/awesome.brightness-widget>

--]]
local wibox = require("wibox")
local awful = require("awful")
local naughty = require("naughty")
local helpers = require("widgets/helpers")
math = require("math")
string = require("string")

local config = awful.util.getdir("config")
local widget = { mt = {}, wmt = {} }

widget.wmt.__index = widget
widget.__index = widget

-- {{{ Create brightness widget method
function widget:new(args)
   local obj = setmetatable({}, self)
   local defaultBlPath = "/sys/class/backlight/acpi_video0"
   local amdGpuBlPath = "/sys/class/backlight/amdgpu_bl0"
   local gmuxBlPath = "/sys/class/backlight/gmux_backlight"
   local intelBlPath = "/sys/class/backlight/intel_backlight"

   obj.cmd = args.cmd or "xbacklight"
   obj.backlightPath = args.backlight_path or defaultBlPath

   if helpers:test(obj.cmd) then
      obj.step = args.step or 5
      obj.incVal = args.inc or "-inc"
      obj.decVal = args.dec or "-dec"
      obj.setVal = args.set or "-set"
      obj.getVal = args.get or "-get"
      obj.backlightPath = nil

   elseif helpers:exists(intelBlPath) then
      obj.backlightPath = intelBlPath

   elseif helpers:exists(gmuxBlPath) then
      obj.backlightPath = gmuxBlPath

   elseif helpers:exists(amdGpuBlPath) then
      obj.backlightPath = amdGpuBlPath

   else
      obj.cmd = nil
      obj.backlightPath = nil
   end

   if helpers:exists(obj.backlightPath) then
      obj.max = helpers:run("cat "..obj.backlightPath.."/max_brightness")
      obj.step = (obj.max / 100) * 3
      obj.cmd = nil
   end

   -- Create imagebox widget
   obj._icon = wibox.widget.imagebox()
   -- Change the draw method so icons can be drawn smaller
   --helpers:set_draw_method(obj.icon)
   -- icon raw path
   obj.iconpath = config.."/theme/icons/brightness-symbolic.svg"
   obj._icon:set_image(obj.iconpath)
   obj.icon = helpers:set_draw_method(obj._icon)

   -- Add a popup
   obj.popup = nil
   obj.brightness = nil

   helpers:listen(obj, 60)

   --  Listen if signal was found
   obj._icon:connect_signal("mouse::enter", function() obj:popupShow() end)
   obj._icon:connect_signal("mouse::leave", function() obj:popupHide() end)


   obj:update()

   return obj
end

function widget:tooltipText()
   return self:get().."% Brightness"
end

function widget:percentage(value)
   return math.floor((100 * value) / self.max)
end

function widget:update()
   local brightness = self:get()
   local iconpath = config.."/theme/icons/status/brightness"

   if(brightness < 5) then
      iconpath = iconpath .. "-none"

   elseif(brightness < 10) then
      iconpath = iconpath .. "-verylow"

   elseif(brightness < 25) then
      iconpath = iconpath .. "-low"

   elseif(brightness < 75) then
      iconpath = iconpath .. "-medium"

   elseif(brightness < 90) then
      iconpath = iconpath .. "-high"

   else
      iconpath = iconpath .. "-full"

   end

   self.iconpath = iconpath .. "-symbolic.svg"

   if self.brightness ~= brightness and self.brightness ~= nil then
      self:popupShow(1)
   end

   self._icon:set_image(self.iconpath)
   self.icon = helpers:set_draw_method(self._icon)

   self.brightness = brightness
end

function widget:up()
   if self.cmd ~= nil then
      helpers:run(self.cmd.." "..self.incVal.." "..self.step)
   elseif self.backlightPath ~= nil then
      local val = math.floor(helpers:run("cat "..self.backlightPath.."/brightness") + self.step)

      if val > math.floor(self.max) or self.brightness > 95 or self:percentage(val) >= 97 then
         val = self.max
      end

      local result = helpers:run("echo "..val.." > "..self.backlightPath.."/brightness")

   end
   helpers:delay(function() self:update() end, 0.1)
end

function widget:down()
   if self.cmd ~= nil then
      helpers:run(self.cmd.." "..self.decVal.." "..self.step)
   elseif self.backlightPath ~= nil then
      local val = math.floor(helpers:run("cat "..self.backlightPath.."/brightness") - self.step)

      if self:percentage(val) <= 5 then
         val = 0
      end
      helpers:run("echo "..val.." > "..self.backlightPath.."/brightness")
   end
   helpers:delay(function() self:update() end, 0.1)
end

function widget:get()
   if self.cmd ~= nil then
      return math.floor(helpers:run(self.cmd.." "..self.getVal) or 0)
   elseif self.backlightPath ~= nil then
      local value = helpers:run("cat "..self.backlightPath.."/brightness")
      return self:percentage(value)
   else
      return 100
   end
end

function widget:set(val)
   if self.cmd ~= nil then
      helpers:run(self.cmd.." "..self.setVal.." "..val)
   elseif self.backlightPath ~= nil then
      val = (self.max / 100) * val;
      helpers:run("echo "..val.." > "..self.backlightPath.."/brightness")
   end
end

function widget:popupShow(timeout)
   local icon = self.iconpath
   local tooltipText = self:tooltipText()
   self:popupHide()
   self.popup = naughty.notify({
         icon = icon,
         icon_size = 16,
         text =  tooltipText,
         timeout = timeout, hover_timeout = 0.5,
         screen = mouse.screen,
         ignore_suspend = true
   })
end

function widget:popupHide()
   if self.popup ~= nil then
      naughty.destroy(self.popup)
      self.popup = nil
   end
end

function widget.mt:__call(...)
   return widget.new(...)
end

return widget:new({})
