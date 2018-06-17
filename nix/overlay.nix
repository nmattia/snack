_: pkgs: {
  snack-lib = pkgs.callPackage ../snack-lib/default.nix { };
  snack-exe = pkgs.writeScriptBin
    "snack"
    (builtins.replaceStrings
      ["NIX_BUILD=nix-build"  "WRAPPER_NIX="]
      ["NIX_BUILD=${pkgs.nix}/bin/nix-build"  "WRAPPER_NIX=${../snack-lib/wrapper.nix}"]
      (builtins.readFile ../bin/snack));
}
