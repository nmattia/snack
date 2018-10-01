{}:
let
  spec = builtins.fromJSON (builtins.readFile ../../../nix/nixpkgs/nixpkgs-src.json);
  nixpkgs =
        (builtins.fetchTarball
            { url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
              sha256 = spec.sha256;
            });
in
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

