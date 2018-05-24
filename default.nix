{ pkgs ? import ./nix {} }:

{ snack-lib = import ./snack/default.nix { inherit pkgs; };
  snack-exe = pkgs.writeScriptBin "snack"
      ''
        set -euo pipefail

        SNACK_NIX=snack.nix

        while [[ $# -gt 0 ]]
        do
        key="$1"

        case $key in
            -f|--snack-nix)
            SNACK_NIX="$2"
            shift
            shift
            ;;
            run|build|ghci)
            COMMAND="$1"
            shift # past argument
            ;;
            *)    # unknown option
            echo "unknown option: $1"
            exit 1
            ;;
        esac
        done
        echo "snack nix: $SNACK_NIX"
        echo "command: $COMMAND"

        case $COMMAND in
          build)
            ${pkgs.nix}/bin/nix-build --no-out-link -A build $SNACK_NIX
            ;;
          ghci)
            res=$(${pkgs.nix}/bin/nix-build --no-out-link -A ghci $SNACK_NIX)
            $res/bin/ghci
            ;;
          run)
            res=$(${pkgs.nix}/bin/nix-build --no-out-link -A build $SNACK_NIX)
            echo "running $res"
            $res/out
            ;;
        esac
      '';
}

