let
  pkgs = import ../../nix {};
  snack = pkgs.snack-lib.snack;
in
  snack
    { main = "Main";
      src = ./.;
      dependencies = ["file-embed"];
      extra-files =
        (modName: if modName == "Main" then [ ./foo.txt ] else []);
    }
