{ inputs
, runCommand
, emanote
}:

let
  nix-filter = inputs.nix-filter.lib;
in
runCommand "website"
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
''
