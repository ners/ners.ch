{ runCommand
, writeShellApplication
, buildEnv
, emanote
}:

let
  name = "website";
  website = runCommand name { nativeBuildInputs = [ emanote ]; } ''
    mkdir -p $out
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
buildEnv {
  inherit name;
  paths = [ website serve ];
}
