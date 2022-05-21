{
  nixConfig = {
    extra-substituters = "https://cache.garnix.io";
    extra-trusted-public-keys =
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=";
  };

  inputs = {
    emanote.url = "github:srid/emanote";
    nixpkgs.follows = "emanote/nixpkgs";
    flake-utils.follows = "emanote/flake-utils";
  };

  outputs = inputs:
    with inputs;
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        defaultPackage = pkgs.runCommand "website"
          {
            buildInputs = [ emanote.defaultPackage.${system} ];
          } ''
          mkdir -p $out
          emanote gen $out
        '';

        defaultApp = pkgs.writeShellApplication {
          name = "emanote";
          text = ''
            set -xe
            export PORT="''${PORT:-8000}"
            emanote run --port "$PORT"
          '';
          runtimeInputs = [ emanote.defaultPackage.${system} ];
        };

        devShell = pkgs.mkShell {
          buildInputs =
            [ pkgs.nixpkgs-fmt emanote.defaultPackage.${system} ];
        };
      });
}
