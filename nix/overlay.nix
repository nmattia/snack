_: pkgs: rec {
  snack-lib = pkgs.callPackage ../snack-lib/default.nix { };
  snack-exe =
    (snack-lib.buildAsExecutable (snack-lib.snackSpec ../bin/snack.nix)).out;
}
