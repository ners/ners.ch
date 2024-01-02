Direnv allows you to execute code when moving around directories in the shell.

Paired with [[Nix]], we can use it to seamlessly enter [[Reproducibility|reproducible]] development shells.

## Enable direnv in your shell
- system-wide with [[NixOS]] or user-wide with [[HomeManager]]
  ```nix
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
  ```
- other systems with Nix
  ```bash
  $ nix profile install nixpkgs\#{direnv,nix-direnv}
  $ echo 'eval "$(direnv hook bash)"' >> ~/.bashrc
  $ echo 'eval "$(direnv hook zsh)"' >> ~/.zshrc
  $ mkdir -p ~/.config/direnv
  $ echo 'source $HOME/.nix-profile/share/nix-direnv/direnvrc' >> ~/.config/direnv/direnvrc
  ```

## Use direnv in a project with a flake

> [!note]
> Ensure that you have [[Flakes]] enabled.

```bash
$ echo use flake > .envrc
$ direnv allow
```
