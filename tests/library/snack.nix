let
  pkgs = import ../../nix {};
  snack = pkgs.snack-lib;
  my-lib = snack.library
    { src = ./src;
      #dependencies = [ "conduit" ];
    };
in
  snack.executable
    { main = "Foo";
      src = ./app;
      dependencies = [ my-lib "conduit" ];
    }
