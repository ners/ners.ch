# Neovim for Haskell

<img class="header-logo" style="max-height: 6em;" src="https://raw.githubusercontent.com/mrcjkb/haskell-tools.nvim/master/nvim-haskell.svg"/>

[[Neovim]] is the best [[Haskell]] editor, thanks in large part to [Marc Jakobi]'s excellent [haskell-tools] plugin.

If you [[NeovimWithNix|configure your Neovim with Nix]] you can use this snippet to get started quickly.

> [!note]
> Please consider :star: starring [haskell-tools] on GitHub. Developers appreciate it!

{ data-filename="neovim/plugins/haskell.nix" }
```nix
{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.unstable.vimPlugins; [
    haskell-tools-nvim
  ];

  xdg.configFile."nvim/ftplugin/haskell.lua".text = ''
    local ht = require('haskell-tools')
    local bufnr = vim.api.nvim_get_current_buf()
    local opts = { noremap = true, silent = true, buffer = bufnr, }

    ht.lsp.start()
  '';
}
```

[Marc Jakobi]: https://mrcjkb.dev/
[haskell-tools]: https://github.com/mrcjkb/haskell-tools.nvim
