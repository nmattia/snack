self: super: {
  snack-lib-with = import ../snack super;
  snack-lib = self.snack-lib-with { };
  snack = super.writeScriptBin "snack"
    "${super.nix}/bin/nix-build snack.nix";
}
