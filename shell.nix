{ pkgs ? import ./nix {} }:
let
  snack = (pkgs.callPackage ./. {}).snack-exe;
in pkgs.mkShell
  { name = "snack-shell";
    buildInputs = [ snack ];
  }
