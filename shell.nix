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
  updateCss = writeShellApplication {
    name = "updateCss";
    text = ''
      date=$(date --iso-8601=seconds | sed 's/[^0-9]//g')
      sed -i "s/\.css\?version=[0-9]\+/\.css\?version=''${date}/" -- **/*.tpl **/*.html
    '';
  };
in
mkShell {
  nativeBuildInputs = [ emanote deploy updateCss ];
}
