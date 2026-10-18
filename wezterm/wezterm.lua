local wezterm = require 'wezterm'
local config = wezterm.config_builder()

-- Start with a maximized window.
wezterm.on('gui-startup', function(cmd)
  local _, _, window = wezterm.mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

config.font = wezterm.font 'JetBrains Mono'
config.font_size = 14.0
config.color_scheme = 'Dracula (Official)'

return config
