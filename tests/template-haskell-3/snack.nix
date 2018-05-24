let
  pkgs = import ../../nix {};
  snack = pkgs.snack-lib;
in
  snack.executable
    { main = "Main";
      src = ./.;
      dependencies = ["file-embed"];
      extra-files =
        # "assets/foo.txt", which is just
        # filterSource (p == "assets/foo.txt") ./.
        (modName: if modName == "Main" then [ "assets/foo.txt" ] else []);
    }
