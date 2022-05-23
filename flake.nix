{
  inputs = {
    emanote.url = "github:ners/emanote";
    nixpkgs.follows = "emanote/nixpkgs";
    flake-utils.follows = "emanote/flake-utils";
  };

  outputs = inputs:
    with inputs;
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        defaultPackage = pkgs.stdenv.mkDerivation {
          name = "website";
          nativeBuildInputs = [ emanote.defaultPackage.${system} ];
          src = ./.;
          phases = [ "unpackPhase" "buildPhase" ];
          buildPhase = ''
            mkdir -p $out
            ${pkgs.exa}/bin/exa
            emanote gen $out
          '';
        };

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
