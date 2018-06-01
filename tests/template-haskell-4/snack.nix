let
  pkgs = import ../../nix {};
  snack = pkgs.snack.snack-lib;
in
  snack.executable
    { main = "Main";
      src = ./src;
      dependencies = ["file-embed"];
      extra-directories =
        (modName: if modName == "Main" then [ ./assets ] else []);
    }
