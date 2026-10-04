-- Erlang: use ELP (Erlang Language Platform). erlang_ls was archived and
-- nvim-lspconfig dropped its "erlangls" config, which LazyVim's erlang extra
-- still enables.
return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        erlangls = { enabled = false },
        elp = {},
      },
    },
  },
  {
    "mason-org/mason.nvim",
    opts = { ensure_installed = { "elp" } },
  },
}
