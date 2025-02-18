with import <nixpkgs> {};

mkShell {
  dontDetectOcamlConflicts = true;
  packages = [
    (jasmin-compiler.overrideAttrs (o: {
      src = fetchurl {
        url = "https://gitlab.com/jasmin-lang/jasmin-compiler/-/archive/f5b199df65695afd63eaf0cc87feaa2137df8065/jasmin-compiler-f5b199df65695afd63eaf0cc87feaa2137df8065.tar.bz2";
        hash = "sha256-lSPqEQ/FElJQr30kpwLiQkKMrFLIky5Uznw73Djtz44=";
      };
      sourceRoot = "jasmin-compiler-f5b199df65695afd63eaf0cc87feaa2137df8065/compiler";
      preBuild = "ocamlopt -v";
    }))
  ] ++ (with ocaml-ng.ocamlPackages; [
    ocaml findlib angstrom dune_3 ocaml-lsp ocamlformat merlin cmdliner
  ]);
}