-- ~/.config/wezterm/wezterm.lua — Catppuccin Mocha
local wezterm = require 'wezterm'
local config = wezterm.config_builder()

-- Tema y fuente (combinan con xmonad/polybar)
config.color_scheme = 'Catppuccin Mocha'
config.font = wezterm.font('JetBrainsMono Nerd Font', { weight = 'Regular' })
config.font_size = 11.0

-- Ventana: sin decoraciones (xmonad dibuja los bordes)
config.window_decorations = 'NONE'
config.window_padding = { left = 8, right = 8, top = 6, bottom = 6 }
config.window_background_opacity = 0.95  -- picom hace la transparencia
config.adjust_window_size_when_changing_font_size = false

-- Pestañas: barra simple abajo, oculta con una sola pestaña
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.hide_tab_bar_if_only_one_tab = true
config.colors = {
  tab_bar = {
    background = '#1e1e2e',
    active_tab = { bg_color = '#f5c2e7', fg_color = '#1e1e2e' },
    inactive_tab = { bg_color = '#313244', fg_color = '#cdd6f4' },
    inactive_tab_hover = { bg_color = '#45475a', fg_color = '#cdd6f4' },
    new_tab = { bg_color = '#1e1e2e', fg_color = '#6c7086' },
    new_tab_hover = { bg_color = '#313244', fg_color = '#cdd6f4' },
  },
}

-- Cursor y comportamiento
config.default_cursor_style = 'SteadyBar'
config.scrollback_lines = 10000
config.audible_bell = 'Disabled'
config.check_for_updates = false  -- se actualiza con dnf (COPR)

return config
