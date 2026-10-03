-- ~/.config/wezterm/dropterm.lua — dropdown terminal (alternative to terminator)
-- Inherits wezterm.lua and applies terminator's high-contrast theme.
-- Sends Ctrl/Shift+Enter (CSI u) so they work in Claude Code through tmux.
local wezterm = require 'wezterm'
local config = dofile(wezterm.config_dir .. '/wezterm.lua')

config.color_scheme = nil
config.colors = {
  foreground = '#f4f6ff',
  background = '#0b0b12',
  cursor_bg = '#f5c2e7',
  cursor_fg = '#0b0b12',
  cursor_border = '#f5c2e7',
  selection_bg = '#f5c2e7',
  selection_fg = '#0b0b12',
  ansi = { '#45475a', '#ff7aa2', '#a6e3a1', '#f9e2af', '#89b4fa', '#f5c2e7', '#94e2d5', '#dce0f0' },
  brights = { '#6c7086', '#ff9ab8', '#c3f5be', '#fff0c4', '#a9c8ff', '#ffd6f1', '#b6f2e6', '#ffffff' },
}
config.window_background_opacity = 1.0
config.enable_tab_bar = false
config.window_padding = { left = 4, right = 4, top = 2, bottom = 2 }
config.enable_csi_u_key_encoding = true

return config
