let
  pkgs = import ../../nix {};
  snack = pkgs.snack-lib;
in
  snack.executable
    { main = "Foo";
      src = ./src;
      dependencies = ["conduit"];
    }
