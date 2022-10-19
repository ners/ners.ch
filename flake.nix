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
        flake-compat.follows = "flake-compat";
        flake-parts.follows = "flake-parts";
        flake-utils.follows = "flake-utils";
        haskell-flake.follows = "haskell-flake";
        nixpkgs.follows = "nixpkgs";
        tailwind.follows = "tailwind-haskell";
      };
    };
    tailwind-haskell = {
      url = "github:srid/tailwind-haskell";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    flake-utils.url = "github:numtide/flake-utils";
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
          overlays = [ emanote-overlay ];
        };
      in
      {
        packages.default = pkgs.callPackage ./default.nix {};
        devShells.default = pkgs.callPackage ./shell.nix {};
        formatter = pkgs.nixpkgs-fmt;
      });
}
