_: pkgs: rec {
  snack-lib = pkgs.callPackage ../snack-lib/default.nix { };
  snack-exe = (snack-lib.executable (import ../bin/snack.nix { inherit (pkgs) writeTextFile symlinkJoin runCommand;})).build.out;
}
