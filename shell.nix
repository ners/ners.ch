{ mkShell
, emanote
}:

mkShell {
  nativeBuildInputs = [ emanote ];
}
