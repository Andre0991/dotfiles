return {
  "julienvincent/nvim-paredit",
  config = function()
    require("nvim-paredit").setup({
      -- Change some keys
      keys = {
        ["<localleader>o"] = false,
        ["<localleader>r"] = { require("nvim-paredit").api.raise_form, "Raise form" },
      },
    })
  end
}
