{ runCommand
, nix-filter
, writeShellApplication
, buildEnv
, emanote
}:

let
  name = "website";
  website = runCommand name
    {
      src = nix-filter {
        root = ./.;
        include = [
          ./static
          ./templates
          nix-filter.isDirectory
          (nix-filter.matchExt "md")
          (nix-filter.matchExt "yaml")
        ];
        exclude = [
          ./.github
        ];
      };
      nativeBuildInputs = [ emanote ];
    } ''
    mkdir -p $out
    cd $src
    emanote gen $out
  '';
  serve = writeShellApplication {
    inherit name;
    text = ''
      export PORT="''${PORT:-8000}"
      emanote run --port "$PORT"
    '';
    runtimeInputs = [ emanote ];
  };
in
buildEnv
{
  inherit name;
  paths = [ website serve ];
}
