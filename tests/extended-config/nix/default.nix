let
  spec =
    let versions = builtins.fromJSON (builtins.readFile ../../../nix/versions.json);
    in versions.nixpkgs;
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

