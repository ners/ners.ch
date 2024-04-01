# Using Nix for Haskell projects

[[Nix]] is the best way to develop and build [[Haskell]] projects.

## Creating a new project

Initialise a new Cabal project by running this command:

```bash
$ nix shell nixpkgs\#{cabal-install,ghc} --command cabal init --interactive
```

Then create the following [[Flakes|Nix flake]]:

{ data-filename="flake.nix" }
```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = inputs:
    with builtins;
    let
      pname = ""; # EDIT THIS
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      hsSrc = root: inputs.nix-filter {
        inherit root;
        include = with inputs.nix-filter.lib; [
          (matchExt "cabal")
          (matchExt "hs")
          (matchExt "md")
          isDirectory
        ];
      };
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = lib.composeExtensions prev.haskell.packageOverrides (hfinal: hprev:
            with prev.haskell.lib.compose;
            {
              "${pname}" = hfinal.callCabal2nix pname (hsSrc ./.) { };
            }
          );
        };
      };
    in
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          ghcs = [ "ghc92" "ghc94" "ghc96" ];
          hps = lib.filterAttrs (ghc: _: elem ghc ghcs) pkgs.haskell.packages;
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = pkgs.haskellPackages.${pname};
          checks.${system}.${pname} = pkgs.buildEnv {
            name = pname;
            paths = map (hp: hp.${pname}) (attrValues hps);
          };
          devShells.${system} =
            foreach (hps // { default = pkgs.haskellPackages; }) (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: [ ps.${pname} ];
                nativeBuildInputs = with hp; [
                  cabal-install
                  fourmolu
                  haskell-language-server
                ];
              };
            });
        }
      ) // {
      overlays.default = overlay;
    };
```

Now you're probably thinking:

## Whoa, that's a big flake!

It is, but it has a number of nice features:
 - support for all Nixpkgs-provided systems (as defined in `nixpkgs.legacyPackages`)
 - support for multiple GHC versions[^1]
 - a devshell that actually works out of the box
 - a flake check for easy CI testing with `nix flake check`
 - an overlay for easy flake composability

## Basic operations

- add Haskell modules and dependencies to the generated Cabal file
    - if using [[Direnv]], make sure to `direnv reload` after changing the Cabal fille to update the devshell
- build and run the project locally with `cabal build` and `cabal run`

[^1]: To support all major GHC versions automatically:
      ```nix
      ghcs = filter (x: match "ghc[0-9]{2}" x != null) (attrNames pkgs.haskell.packages);
      ```
