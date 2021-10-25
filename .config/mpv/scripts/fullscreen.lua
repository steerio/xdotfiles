function cycle_fs_di_handler()
  local value = not mp.get_property_bool("fullscreen")
  mp.osd_message(value and
    "Total TV immersion!" or
    "Focusing on work. Good.")
  mp.set_property_bool("fullscreen", value)
  mp.set_property_bool("deinterlace", value)
end

mp.add_key_binding("F", "cycle_fs_di", cycle_fs_di_handler)
