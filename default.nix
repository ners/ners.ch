{ lib
, runCommand
, emanote
}:

runCommand "website"
{
  src = with builtins; with lib.fileset; toSource {
    root = ./.;
    fileset = fileFilter
      (file: any file.hasExt [
        "css"
        "gif"
        "hs"
        "html"
        "jpg"
        "lua"
        "md"
        "png"
        "svg"
        "tpl"
        "ttf"
        "woff2"
        "yaml"
      ]) ./.;
  };
  nativeBuildInputs = [ emanote ];
} ''
  mkdir -p $out
  cd $src
  emanote gen $out
''
