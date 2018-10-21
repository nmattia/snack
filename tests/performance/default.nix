let
  pkgs = import ../../nix {};
in pkgs.snack-lib.inferSnackBuild ./package.nix
