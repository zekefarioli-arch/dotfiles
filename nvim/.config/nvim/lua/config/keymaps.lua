-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

-- Next to LazyVim's other toggles (<leader>uw wrap, <leader>ul line numbers)
Snacks.toggle.option("list", { name = "Whitespace" }):map("<leader>uW")
