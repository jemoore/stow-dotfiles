-- Markdown customizations on top of the lazyvim.plugins.extras.lang.markdown extra.
-- The extra provides: marksman (LSP), markdownlint-cli2 (lint),
-- render-markdown.nvim (in-buffer rendering), markdown-preview.nvim (<leader>cp).
return {
  {
    "MeanderingProgrammer/render-markdown.nvim",
    opts = {
      heading = { position = "inline" },
      code = { width = "block", right_pad = 2 },
      checkbox = {
        unchecked = { icon = "󰄱 " },
        checked = { icon = "󰱒 " },
      },
    },
  },

  -- Inline image rendering (kitty graphics protocol; needs ImageMagick)
  {
    "folke/snacks.nvim",
    opts = {
      image = {},
    },
  },

  -- Live table alignment (:TableModeToggle)
  {
    "dhruvasagar/vim-table-mode",
    ft = "markdown",
    init = function()
      vim.g.table_mode_corner = "|" -- markdown-style separators, not org-style "+"
    end,
  },

  -- Auto-continue and renumber bullet/numbered lists on Enter
  {
    "bullets-vim/bullets.vim",
    ft = { "markdown", "text", "gitcommit" },
  },
}
