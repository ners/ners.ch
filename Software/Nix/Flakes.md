Flakes are an [experimental feature][experimental] of [[Nix]], introduced in version 2.4.

To use flakes, you must enable them in your configuration:
 - on [[NixOS]] or any system with [[HomeManager|Home Manager]]:
   ```nix
   nix.settings.experimental-features = [ "nix-command" "flakes" ];
   ```
 - on any other system, run the following commands:
   ```bash
   mkdir -p ~/.config/nix
   echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf
   ```

## What is a Nix Flake?

A Nix Flake is a file named flake.nix placed at the root of a project or a repository.

> [!info]
> Before continuing, ensure you are familiar with the [Nix language basics].

A Nix Flake contains a Nix attribute set with a schema (certain expected properties).
At minimum, it must contain the following attributes:
 - `inputs`, an attribute set
 - `outputs`, a function from resolved inputs to an attribute set.

A minimum valid flake:
```nix
{
  inputs = { };
  outputs = inputs: { };
}
```

[Nix language basics]: https://nix.dev/tutorials/nix-language
[experimental]: https://nixos.org/manual/nix/stable/contributing/experimental-features.html
