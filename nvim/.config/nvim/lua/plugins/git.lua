-- Git workflow on top of LazyVim's defaults (gitsigns, lazygit, snacks git pickers):
-- inline blame on the current line and diffview for diffs and file history.
return {
  {
    "lewis6991/gitsigns.nvim",
    opts = {
      current_line_blame = true,
      current_line_blame_opts = { delay = 500 },
    },
  },

  {
    "sindrets/diffview.nvim",
    cmd = { "DiffviewOpen", "DiffviewClose", "DiffviewFileHistory" },
    keys = {
      { "<leader>gvo", "<cmd>DiffviewOpen<cr>", desc = "Diffview: working tree" },
      { "<leader>gvc", "<cmd>DiffviewClose<cr>", desc = "Diffview: close" },
      { "<leader>gvf", "<cmd>DiffviewFileHistory %<cr>", desc = "Diffview: file history" },
      { "<leader>gvr", "<cmd>DiffviewFileHistory<cr>", desc = "Diffview: repo history" },
      { "<leader>gvm", "<cmd>DiffviewOpen origin/HEAD...HEAD<cr>", desc = "Diffview: branch vs default" },
    },
    opts = { enhanced_diff_hl = true },
  },

  {
    "folke/which-key.nvim",
    opts = { spec = { { "<leader>gv", group = "diffview", icon = { icon = " ", color = "orange" } } } },
  },
}
