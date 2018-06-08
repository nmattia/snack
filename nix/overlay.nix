_: pkgs: {
  snack-lib = pkgs.callPackage ../snack/default.nix { };
  snack-exe = pkgs.writeScriptBin
    "snack"
    (builtins.replaceStrings
      ["NIX_BUILD=nix-build"]
      ["NIX_BUILD=${pkgs.nix}/bin/nix-build"]
      (builtins.readFile ../bin/snack));
}
