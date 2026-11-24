local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.front_end = 'WebGpu'

-- Start with a maximized window.
wezterm.on('gui-startup', function(cmd)
  local _, _, window = wezterm.mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

config.font = wezterm.font 'Iosevka Extended'
config.font_size = 14
config.color_scheme = 'Dracula (Official)'

-- Forces WezTerm to match standard wcwidth/runewidth cell calculations
config.unicode_version = 16
config.treat_east_asian_ambiguous_width_as_wide = false


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

-- Work around agy redraw glitches caused by its cursor-optimization escape sequences.
config.set_environment_variables = {
  AGY_CLI_DISABLE_ESCAPE_SEQUENCE_OPTIMIZATIONS = '1',
}

return config
