-- Front end: HTML, CSS and Emmet on top of the TypeScript/Tailwind/Vue/Svelte extras
return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        html = {},
        cssls = {},
        emmet_language_server = {
          filetypes = { "html", "css", "scss", "javascriptreact", "typescriptreact", "vue", "svelte", "eelixir", "heex" },
        },
      },
    },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "html", "css", "scss", "javascript", "tsx" } },
  },
}
