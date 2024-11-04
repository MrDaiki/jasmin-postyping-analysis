{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/7881fbfd2e3ed1dfa315fca889b2cfd94be39337.tar.gz";
    sha256 = "sha256:0na5zykmva0a6valzrrcigp6g0rzq28mi7dqxqr0s3jbn6fm24hq";
  }) {}
}:

with pkgs;

mkShell{
    packages = with ocamlPackages; [ocaml findlib dune_3 jasmin-compiler ocaml-lsp merlin ocamlformat];
}