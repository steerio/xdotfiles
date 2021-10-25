mp.input_define_section("stream",
[[WHEEL_UP ignore
WHEEL_DOWN ignore
RIGHT ignore
LEFT ignore
UP ignore
DOWN ignore]])

local function changed(name, value)
  if value and string.find(value, "rtsp://") == 1 then
    mp.input_enable_section("stream")
  else
    mp.input_disable_section("stream")
  end
end

mp.observe_property("stream-open-filename", "string", changed)
