{pkgs ? import <nixos-unstable> {}}:
pkgs.mkShell {
  buildInputs = with pkgs.ocamlPackages; [
    ocaml
    utop
    findlib
    zarith
    alcotest
    containers
    menhir
    sedlex
    js_of_ocaml
    js_of_ocaml-compiler
    dune_2
  ];
}

