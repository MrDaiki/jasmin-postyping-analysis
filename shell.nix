with import <nixpkgs> {};

mkShell {
  dontDetectOcamlConflicts = true;
  packages = [
    (jasmin-compiler.overrideAttrs (o: {
      src = fetchurl {
        url = "https://github.com/jasmin-lang/jasmin/releases/download/v2025.02.0/jasmin-compiler-v2025.02.0.tar.bz2";
        hash = "sha256-xLTMdyFyZGlnhuZ1iOg8Tgm7aLijg5lceJxgLdEHw+Q=";
      };
    }))
  ] ++ (with ocaml-ng.ocamlPackages; [
    ocaml findlib angstrom dune_3 ocaml-lsp ocamlformat merlin cmdliner yojson ppx_yojson_conv
  ]);
}