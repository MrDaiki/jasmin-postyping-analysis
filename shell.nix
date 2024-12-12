with import <nixpkgs> {};

mkShell {
  dontDetectOcamlConflicts = true;
  packages = [
    (jasmin-compiler.overrideAttrs (o: {
      src = fetchurl {
        url = "https://gitlab.com/jasmin-lang/jasmin-compiler/-/archive/release-2024.07/jasmin-compiler-release-2024.07.tar.bz2";
    hash = "sha256-kgNbs/8r2g5oX5FITnuRjvIhDp8cX/QVAsjfTYcvSRI=";
      };
      sourceRoot = "jasmin-compiler-release-2024.07/compiler";
      preBuild = "ocamlopt -v";
    }))
  ] ++ (with ocaml-ng.ocamlPackages; [
    ocaml findlib angstrom dune_3 ocaml-lsp ocamlformat merlin
  ]);
}