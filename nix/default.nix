{ nixpkgs ? ./nixpkgs }:
import (import nixpkgs) {
  config = { allowUnfree = true; };
  overlays = [
    (self: super: { snack-lib = import ../snack;} )
    (self: super: { snack = self.writeScriptBin "snack"
      ''
        ${self.nix}/bin/nix-build snack.nix
      ''; })
  ];
}
