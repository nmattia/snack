{ pkgs ? import ./nix {} }:
with pkgs;
mkShell {
  buildInputs = [ snack-exe ];
}
