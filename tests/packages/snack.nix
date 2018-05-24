let
  pkgs = import ../../nix {};
  snack = pkgs.snack.snack-lib;
in
  snack.executable
    { main = "Foo";
      src = ./src;
      dependencies = ["conduit"];
    }
