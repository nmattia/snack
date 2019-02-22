with { pkgs = import ./nix {}; };
{
  inherit (pkgs.packages)
    snack-lib
    snack-exe
    ;
}
