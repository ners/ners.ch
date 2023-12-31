{ mkShell
, emanote
, writeShellApplication
}:

let
  deploy = writeShellApplication {
    name = "deploy";
    text = ''
      source=$(nix build --print-out-paths --no-link)
      for target in "$@"; do
        echo "deploying to $target"
        rsync -av "$source/" "$target/"
      done
    '';
  };
in
mkShell {
  nativeBuildInputs = [ emanote deploy ];
}
