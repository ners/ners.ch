{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    haskell-flake.url = "github:srid/haskell-flake";
    ema = {
      url = "github:srid/ema";
      inputs = {
        emanote.follows = "emanote";
        flake-parts.follows = "flake-parts";
        flake-root.follows = "flake-root";
        haskell-flake.follows = "haskell-flake";
        nixpkgs.follows = "nixpkgs";
        treefmt-nix.follows = "treefmt-nix";
      };
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emanote = {
      url = "github:srid/emanote";
      inputs = {
        ema.follows = "ema";
        flake-parts.follows = "flake-parts";
        flake-root.follows = "flake-root";
        haskell-flake.follows = "haskell-flake";
        nixpkgs.follows = "nixpkgs";
        treefmt-nix.follows = "treefmt-nix";
      };
    };
    flake-root.url = "github:srid/flake-root";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${builtins.typeOf xs}"
      );
      emanote-overlay = _: prev: {
        emanote = inputs.emanote.packages.${prev.system}.default;
      };
    in
    foreach inputs.nixpkgs.legacyPackages (system: pkgs':
      let pkgs = pkgs'.extend emanote-overlay; in
      {
        legacyPackages.${system} = pkgs;
        packages.${system}.default = pkgs.callPackage ./default.nix { };
        devShells.${system}.default = pkgs.callPackage ./shell.nix { };
        formatter.${system} = pkgs.nixpkgs-fmt;
      });
}
