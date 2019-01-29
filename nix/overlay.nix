_: pkgs: rec {
  snack-lib = pkgs.callPackage ../snack-lib/default.nix { };
  snack-exe = snack-lib.executable ../bin/package.nix;
}
