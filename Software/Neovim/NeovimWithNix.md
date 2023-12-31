# Configure your Neovim with Nix

Neovim is rather barebones out of the box. This may make some people think it is only useable for trivial tasks.

However, there is a thriving plugin ecosystem that can extend Neovim with many IDE-like features.

After investing time into making it your own, the best way to make your config [[Nix/Reproducibility|reproducible]] is with [[Nix/HomeManager]].

> [!info]
> This approach is extracted from [Trilby]. See its [Neovim configuration][Trilby-neovim] for more.

## The basics

We want to build up our config in a maintainable fashion.

We can leverage [[Nix]] to split up a nontrivial config and observe the [[Software/SRP]].

Assuming that your Home Manager is configured in a file named `home.nix`, create a directory `neovim/` adjacent to it, and import it as follows:

{ data-filename="home.nix" }
```nix
  imports = [
    ./neovim
  ];
```

{ data-filename="neovim/default.nix" }
```nix
{ lib, ... }:

with builtins;
with lib;
let
  plugins = pipe ./plugins [
    readDir
    attrNames
    (map (fn: ./plugins/${fn}))
  ];
  nixPlugins = filter (hasSuffix ".nix") plugins;
  luaPlugins = filter (hasSuffix ".lua") plugins;
in
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    withPython3 = true;
    extraLuaConfig = lib.mkBefore ''
      -- bytecompile lua modules
      vim.loader.enable()

      -- load .exrc, .nvimrc and .nvim.lua local files
      vim.o.exrc = true
    '';
  };

  # Import all the modules in ./plugins/*.nix
  imports = nixPlugins;

  # Link Lua files from ./plugins/*.lua to ~/.config/nvim/plugin/
  xdg.configFile = pipe luaPlugins [
    (map (fn: { "nvim/plugin/${name}".source = fn; }))
    (foldr recursiveUpdate { })
  ];
}
```

With this in place, we may now populate the directory `neovim/plugins/` with
 - Nix modules that specify plugins and their configs
 - Lua configuration that specifies pure config

## Example plugins

### Nix LSP

{ data-filename="neovim/plugins/lsp.nix" }
```nix
{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.vimPlugins; [
    lsp-inlayhints-nvim
    lsp_extensions-nvim
    lsp_signature-nvim
    lspkind-nvim
    nvim-lspconfig
  ];
}
```

{ data-filename="neovim/plugins/nix.nix" }
```nix
{ pkgs, ... }:

{
  # We use the nil LSP server and nixpkgs-fmt for autoformatting,
  # so we must make sure they are installed
  home.packages = with pkgs; [ nil nixpkgs-fmt ];

  programs.neovim.extraLuaConfig = ''
    require('lspconfig').nil_ls.setup({})
  '';

  xdg.configFile."nvim/ftplugin/nix.lua".text = ''
    vim.bo.expandtab = true
    vim.bo.shiftwidth = 2
    vim.bo.softtabstop = 2
    vim.bo.tabstop = 2
  '';
}
```

### Treesitter

{ data-filename="neovim/plugins/treesitter.nix" }
```nix
{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.vimPlugins; [
    nvim-treesitter.withAllGrammars
    rainbow-delimiters-nvim
  ];
}
```

{ data-filename="neovim/plugins/treesitter.lua" }
```lua
local cfg = require('nvim-treesitter.configs')
cfg.setup {
    highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
        -- disable TS for large buffers because it is slow
        disable = function(_, buf)
            local max_filesize = 100 * 1024 -- 100 KiB
            local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
            if ok and stats and stats.size > max_filesize then
                return true
            end
        end
    },
    rainbow = {
        enable = true,
        query = 'rainbow-parens',
        strategy = require('rainbow-delimiters').strategy['global'],
    },
}
```

[Trilby]: https://github.com/ners/trilby
[Trilby-neovim]: https://github.com/ners/trilby/tree/main/modules/home/neovim
