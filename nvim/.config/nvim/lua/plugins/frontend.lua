-- Front end: HTML, CSS and Emmet on top of the TypeScript/Tailwind/Vue/Svelte extras
return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        -- htmldjango also covers Nunjucks (.njk, GOV.UK Frontend templates; see config/options.lua)
        html = { filetypes = { "html", "templ", "htmldjango" } },
        cssls = {},
        emmet_language_server = {
          filetypes = { "html", "css", "scss", "javascriptreact", "typescriptreact", "vue", "svelte", "eelixir", "heex", "htmldjango" },
        },
      },
    },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "html", "css", "scss", "javascript", "tsx" } },
  },
}
