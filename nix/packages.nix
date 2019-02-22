{ callPackage }:
rec
{
  snack-lib = callPackage ../snack-lib/default.nix { };
  snack-exe = snack-lib.executable ../bin/package.nix;
}
