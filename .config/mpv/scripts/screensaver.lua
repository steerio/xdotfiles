function cycle_screensaver_handler()
  local value = mp.get_property("options/stop-screensaver")
  if value == "yes" then
    value = "no"
    mp.osd_message("Enabling screensaver.")
  else
    value = "yes"
    mp.osd_message("Stopping screensaver.")
  end
  mp.set_property("options/stop-screensaver", value)
end

mp.add_key_binding("Ctrl+s", "cycle_screensaver", cycle_screensaver_handler)
