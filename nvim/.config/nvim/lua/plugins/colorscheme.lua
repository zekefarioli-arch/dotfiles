-- Catppuccin Mocha with pink accents, matching the rest of the desktop
return {
  {
    "catppuccin/nvim",
    name = "catppuccin",
    opts = {
      flavour = "mocha",
      custom_highlights = function(c)
        return {
          CursorLineNr = { fg = c.pink, style = { "bold" } },
          FloatBorder = { fg = c.pink },
          FloatTitle = { fg = c.base, bg = c.pink, style = { "bold" } },
          WinSeparator = { fg = c.surface1 },
          SnacksPickerBorder = { fg = c.pink },
          SnacksPickerTitle = { fg = c.base, bg = c.pink, style = { "bold" } },
          SnacksDashboardHeader = { fg = c.pink },
          SnacksDashboardIcon = { fg = c.pink },
          SnacksDashboardKey = { fg = c.peach },
          WhichKeyBorder = { fg = c.pink },
        }
      end,
    },
  },

  { "LazyVim/LazyVim", opts = { colorscheme = "catppuccin-mocha" } },

  -- Status line: normal mode in pink instead of blue
  {
    "nvim-lualine/lualine.nvim",
    opts = function(_, opts)
      local ok, theme = pcall(require, "lualine.themes.catppuccin")
      if not ok then
        return
      end
      theme = vim.deepcopy(theme)
      local pink = require("catppuccin.palettes").get_palette("mocha").pink
      theme.normal.a.bg = pink
      theme.normal.b.fg = pink
      opts.options = opts.options or {}
      opts.options.theme = theme
    end,
  },
}
