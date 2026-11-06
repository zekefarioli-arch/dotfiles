-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

-- Nunjucks templates ({% %} and {{ }}, used by GOV.UK Frontend) have no filetype of their own;
-- htmldjango has the same syntax and gets HTML and Emmet support from the servers in plugins/frontend.lua.
vim.filetype.add({ extension = { njk = "htmldjango" } })

-- Show whitespace (LazyVim already turns `list` on, but its default listchars leave plain spaces out).
-- Toggle with <leader>uW (config/keymaps.lua).
vim.opt.listchars = { tab = "» ", space = "·", trail = "•", nbsp = "␣" }
