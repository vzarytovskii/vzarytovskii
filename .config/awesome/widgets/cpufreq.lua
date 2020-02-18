--[[

   Awesome WM CPUFreq Widget
   Distopico Vegan <distopico [at] riseup [dot] net>
   Licensed under GPL3

--]]

local awful = require("awful")
local wibox = require("wibox")
local helpers = require("widgets/helpers")

local fraxcpumenu = {}
local fraxcpu = {}
local governor_state = {
   ["ondemand\n"]     = "↯",
   ["powersave\n"]    = "⌁",
   ["userspace\n"]    = "¤",
   ["performance\n"]  = "⚡",
   ["conservative\n"] = "⊚"
}

-- Create fraxcpumenu, and add all available governors to it
local fh= io.open("/sys/devices/system/cpu/cpu0/cpufreq/scaling_available_governors", "r")
if fh ~= nil then
   govstr= fh:read()
   fh:close()
   local i= 1
   for w in string.gmatch(govstr, "%a+") do
      local icon = governor_state[w.."\n"] or ""
      fraxcpumenu[i] = { icon.."  "..w, "sudo cpupower frequency-set -g "..w}
      i= i + 1
   end
end
fraxcpumenu = awful.menu.new( { items = fraxcpumenu, theme = {width = 120} }  )

-- Create fraxcpu widget
fraxcpu.text =  wibox.widget.textbox()

-- Function for updating the fraxcpu widget
fraxcpuupd = 1
function fraxcpu:update ()
   if not fraxcpuupd then return(nil) end
   local _cpufreq = helpers:pathtotable("/sys/devices/system/cpu/cpu0/cpufreq")
   -- Default frequency and voltage values
   local freqv = {
      ["mhz"] = "N/A", ["ghz"] = "N/A",
      ["v"]   = "N/A", ["mv"]  = "N/A",
   }
   -- Get the current frequency and governor
   local freq = tonumber(_cpufreq.scaling_cur_freq)
   local governor = _cpufreq.scaling_governor

   -- Represent the governor as a symbol
   governor = governor_state[governor] or governor or "N/A"

   if freq then
      -- Calculate MHz and GHz
      freqv.mhz = freq / 1000
      freqv.ghz = freqv.mhz / 1000

      -- Get the current voltage
      if _cpufreq.scaling_voltages then
         freqv.mv = tonumber(string.match(_cpufreq.scaling_voltages, freq.."[%s]([%d]+)"))
         -- Calculate voltage from mV
         freqv.v  = freqv.mv / 1000
      end
   else
      fraxcpuupd = nil
      local fh = io.open("/proc/cpuinfo", "r")
      if fh then
         for l in fh:lines() do
            freq = string.match(l, '^%s*cpu MHz%s*:%s*([0-9]+)')
            if freq ~= nil then break end
            freq = nil
         end
      end
   end
   fraxcpu.text:set_markup(string.sub(freqv.ghz, 0, 3).."GHz "..governor)
end

-- Mouse button bindings for fraxcpu widget
fraxcpu.text:buttons(awful.util.table.join(
                        awful.button({ }, 1, function () awful.menu.toggle(fraxcpumenu) end),
                        awful.button({ }, 2, function () fraxcpu:update() end),
                        awful.button({ }, 3, function () fraxcpu:update()  end)
))
-- {{{ Listen if signal was found
if fraxcpuupd then
   helpers:listen(fraxcpu, 5)
end
-- }}

return fraxcpu
