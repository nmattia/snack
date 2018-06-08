let pkgs = import ./nix {}; in
{
  inherit (pkgs)
    snack-lib
    snack-exe
    ;
}
