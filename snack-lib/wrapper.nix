{ snackNix
, nixpkgs ? null
}:
  let
    pkgs =
      if nixpkgs == null
      then import <nixpkgs> {}
      else import nixpkgs {};
    snack = pkgs.snack-lib;
  in
{
  build = (snack.executable (import snackNix)).build;
  ghci = (snack.executable (import snackNix)).ghci;
}
