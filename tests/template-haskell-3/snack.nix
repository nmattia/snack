let
  pkgs = import ../../nix {};
  snack = pkgs.snack-lib;
in
  snack.executable
    { main = "Main";
      src = ./.;
      dependencies = ["file-embed"];
      extra-files =
        (modName: if modName == "Main" then [ "assets/foo.txt" ] else []);
    }
