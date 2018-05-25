{ pkgs ? import ./nix {} }:

{ snack-lib = import ./snack/default.nix { inherit pkgs; };
  snack-exe = pkgs.writeScriptBin
    "snack"
    (builtins.replaceStrings
      ["NIX_BUILD=nix-build"]
      ["NIX_BUILD=${pkgs.nix}/bin/nix-build"]
      (builtins.readFile ./bin/snack));
}

