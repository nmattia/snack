with { pkgs = import ./nix {}; };
pkgs.mkShell {
  buildInputs = [ pkgs.packages.snack-exe pkgs.nix ];
}
