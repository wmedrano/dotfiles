local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.front_end = 'WebGpu'

-- Start with a maximized window.
wezterm.on('gui-startup', function(cmd)
  local _, _, window = wezterm.mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

-- Iosevka Term (not plain Iosevka) keeps every glyph exactly one cell wide;
-- plain Iosevka draws some symbols (e.g. em-dash) double-width, which overlap.
config.font = wezterm.font('Iosevka Term', { stretch = 'Expanded' })
config.font_size = 14
config.color_scheme = 'Dracula (Official)'

-- Forces WezTerm to match standard wcwidth/runewidth cell calculations
config.unicode_version = 16

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
