{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    haskell-flake.url = "github:srid/haskell-flake";
    ema = {
      url = "github:srid/ema";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
        haskell-flake.follows = "haskell-flake";
      };
    };
    emanote = {
      url = "github:srid/emanote";
      inputs = {
        ema.follows = "ema";
        flake-parts.follows = "flake-parts";
        haskell-flake.follows = "haskell-flake";
        nixpkgs.follows = "nixpkgs";
      };
    };
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
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
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        emanote-overlay = self: super: {
          emanote = inputs.emanote.packages.${system}.default;
        };
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.nix-filter.overlays.default emanote-overlay ];
        };
      in
      {
        packages.default = pkgs.callPackage ./default.nix { };
        devShells.default = pkgs.callPackage ./shell.nix { };
        formatter = pkgs.nixpkgs-fmt;
      });
}
