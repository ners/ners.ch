# Separation of concerns

Nix allows us to track dependencies (software that we depend on) at various levels.

Freshly minted NixOS users sometimes despair about how difficult it is to "install" software.

However, most of the time you don't actually need to _install_ anything before you can use it.

## One-off

With Nix it is possible to run a program once, e.g. to try it out, without tracking it anywhere:

```bash
nix run nixpkgs\#epiphany
```

You can also enter a temporary shell with multiple programs:

```bash
$ nix shell nixpkgs\#{epiphany,firefox}
$ epiphany
```

You can also go back in time to run old versions of software:

```bash
nix run nixpkgs/release-21.11\#epiphany
```

Try _that_ with other package managers! 😼

## Project level

If you are working on a project that depends on applications and libraries, you should add them to your project's [[Flakes|flake]]:

```nix
{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = inputs:
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${builtins.typeOf xs}"
      );
    in
    foreach inputs.nixpkgs.legacyPackages (system: pkgs: {
      devShells.${system}.default = pkgs.mkShell [
          nativeBuildInputs = with pkgs; [
            epiphany
            firefox
          ];
        ];
      }
    );
}
```

This way the project [[Reproducibility|correctly tracks its own dependencies]] and can be shared between developers across operating systems.

## User level

If you have applications that you wish to use outside of projects, then the best way to track them is with [[HomeManager]]:

```nix
{ pkgs, ... }:

{
  home.packages = with pkgs; [
    epiphany
    firefox
  ];
}
```

Without Home Manager, a user can set up aliases for their commonly-used applications:

```bash
alias firefox='nix run nixpkgs#firefox --'

alias 7z='nix shell nixpkgs#p7zip --command 7z'
```

These aliases can be made in a temporary session, or persisted in `.rc` files.


## System level

Finally, if you are using [[NixOS]] and wish to make some applications available to all users of the operating system, then they can be tracked on the system level:

```nix
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    epiphany
    firefox
  ];
}
```

The rule of thumb is: if the system is usable without this piece of software, it should probably be tracked on the project or user level.

## Profile mutation

> [!warning]
> This section is only included for completeness. _Don't do this._

It is possible to "install" software on NixOS or any distribution with Nix, in a similar way to how other distributions' package managers work:

```bash
nix profile install nixpkgs\#epiphany
```

or with the old-style command:

```bash
nix-env -iA epiphany
```

However, using `nix profile` and `nix-env` should be avoided:
- https://stop-using-nix-env.privatevoid.net/
