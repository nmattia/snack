self: super: {
  snack-lib-with = import ../snack;
  snack-lib = self.snack-lib-with { pkgs = super; };
  snack = super.writeScriptBin "snack"
    "${super.nix}/bin/nix-build snack.nix";
}
