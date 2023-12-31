# Neovim for Haskell developers

If you [[NeovimWithNix]], then you can use this snippet to set it up for [[Haskell]], using [Marc Jakobi]'s excellent [haskell-tools] plugin.

> [!note]
> If you like a plugin, please consider :star: starring it on GitHub. It costs you nothing and the developers appreciate it!

{ data-filename="neovim/plugins/haskell.nix" }
```nix
{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.unstable.vimPlugins; [
    haskell-tools-nvim
  ];

  xdg.configFile."nvim/ftplugin/haskell.lua".text = ''
    vim.bo.expandtab = true
    vim.bo.shiftwidth = 4
    vim.bo.softtabstop = 4
    vim.bo.tabstop = 4

    local ht = require('haskell-tools')
    local bufnr = vim.api.nvim_get_current_buf()
    local opts = { noremap = true, silent = true, buffer = bufnr, }

    ht.lsp.start()
  '';
}
```

[Marc Jakobi]: https://mrcjkb.dev/
[haskell-tools]: https://github.com/mrcjkb/haskell-tools.nvim
