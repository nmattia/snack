with
rec {
  sources = import ../../../nix/sources.nix;
  nixpkgs = sources.nixpkgs;
};
import nixpkgs
  { overlays =
      [
        (self: super:
          { haskellPackages = super.haskellPackages.extend
               (super.haskell.lib.packageSourceOverrides
                   { something-that-doesnt-exist =
                        self.lib.cleanSource ./extra-hs;
                  }
                );
          }
        )
      ];
  }
