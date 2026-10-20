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

-- Make text like "cr/8604710" clickable as a Chromium Gerrit link.
config.hyperlink_rules = wezterm.default_hyperlink_rules()
table.insert(config.hyperlink_rules, {
  regex = [[\bcr/(\d+)\b]],
  format = 'https://chromium-review.googlesource.com/c/chromium/src/+/$1',
})

-- Free ctrl+shift+space (default: QuickSelect) so it reaches terminal apps.
config.keys = {
  { key = 'Space', mods = 'CTRL|SHIFT', action = wezterm.action.DisableDefaultAssignment },
}

return config
