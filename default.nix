{ pkgs ? import <nixpkgs> {} }:

{ snack-lib = import ./snack/default.nix { inherit pkgs; };
  snack-exe = pkgs.writeScriptBin "snack"
      ''
        set -e
        case $1 in
          build)
            ${pkgs.nix}/bin/nix-build snack.nix
            ;;
          ghci)
            res=$(${pkgs.nix}/bin/nix-build -A ghci snack.nix)
            $res/bin/ghci
        esac
      '';
}

