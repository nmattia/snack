let
  pkgs = import ../../nix {};
  snack = pkgs.snack-lib.snack;
in
  snack
    { main = "Foo";
      src = ./src;
      dependencies = ["conduit"];
    }
