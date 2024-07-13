Flakes are an [experimental feature][experimental] of [[Nix]], introduced in version 2.4.

## Enable flakes
To use flakes, you must enable them in your configuration:
 - system-wide on [[NixOS]] or user-wide with [[HomeManager|Home Manager]]:
   ```nix
   nix.settings.experimental-features = [ "nix-command" "flakes" ];
   ```
 - on any other system, run the following commands:
   ```bash
   mkdir -p ~/.config/nix
   echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf
   ```

## What is a Nix Flake?

A Nix Flake is a file named `flake.nix` placed at the root of a project or a repository.

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

## Benefits of Nix Flakes

A Nix Flake allows easy [[Reproducibility|source pinning]] of external resources. Compare the following expressions:

```nix
{ pkgs ? import <nixpkgs> {}
}:

...

{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/06278c77b5d162e62df170fec307e83f1812d94b.tar.gz") {}
}:

...
```

The problem with the first one is that it depends on [channels], an opaque and stateful mechanism of tracking external dependencies.

The problem with the second one is that the URL of the external resource contains arbitrary commit hashes, and it is not obvious how this should be maintained.

Contrast to a flake:

```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = inputs: {
    let
      system = builtins.currentSystem;
      pkgs = inputs.nixpkgs.legacyPackages.${system};
    in
    ...
  };
}
```

This has several advantages:
 - the external resources (in this case nixpkgs) are listed cleanly
 - the resources are pinned in a separate file called `flake.lock`
 - the lock file can be updated without changing the `flake.nix`
 - we may explicitly define the differences between various host systems

[Nix language basics]: https://nix.dev/tutorials/nix-language
[experimental]: https://nixos.org/manual/nix/stable/contributing/experimental-features.html
[channels]: https://nixos.wiki/wiki/Nix_channels
