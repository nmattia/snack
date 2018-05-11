{ nixpkgs ? ./nixpkgs }:
import (import nixpkgs) {
  config = { allowUnfree = true; };
  overlays = [
    (self: super: { snack-lib = import ../snack {};} )
    (self: super: { snack-lib-with = extra: import ../snack extra;} )
    (self: super: { snack = self.writeScriptBin "snack"
      ''
        set -e
        case $1 in
          build)
            ${self.nix}/bin/nix-build snack.nix
            ;;
          ghci)
            res=$(${self.nix}/bin/nix-build -A ghci snack.nix)
            $res/bin/ghci
        esac
      ''; })
  ];
}
