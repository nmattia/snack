{ snackNix
, nixpkgs ? null
}:
  let
    pkgs =
      if nixpkgs == null
      then import <nixpkgs> {}
      else import nixpkgs {};
    snack = pkgs.snack-lib;
    snackDef = import snackNix;
  in
{
  build = (snack.executable snackDef).build;
  ghci = (snack.executable snackDef).ghci;
}
