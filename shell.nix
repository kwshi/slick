{pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  buildInputs = with pkgs.ocamlPackages; [
    dune_2
    ocaml
    utop
    findlib
    zarith
    fmt
    cmdliner
    alcotest
    containers
    menhir
    sedlex
  ];
}

