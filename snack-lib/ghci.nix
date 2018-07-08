{ makeWrapper, symlinkJoin, lib, callPackage, writeScriptBin }:

with (callPackage ./module-spec.nix {});
with (callPackage ./modules.nix {});

rec {

  # Write a new ghci executable that loads all the modules defined in the
  # module spec
  ghciWithMain = ghcWith: mainModSpec:
    let
      imports = allTransitiveImports [mainModSpec];
      modSpecs = [mainModSpec] ++ imports;
    in ghciWithModules ghcWith modSpecs;

  ghciWithModules = ghcWith: modSpecs:
    let
      ghcOpts = allTransitiveGhcOpts modSpecs
        ++ (map (x: "-X${x}") (allTransitiveExtensions modSpecs));
      ghc = ghcWith (allTransitiveDeps modSpecs);
      ghciArgs = lib.strings.escapeShellArgs
        (ghcOpts ++ absoluteModuleFiles);
      absoluteModuleFiles =
        map
          (mod:
            builtins.toString (mod.moduleBase) +
              "/${moduleToFile mod.moduleName}"
          )
          modSpecs;

      dirs = allTransitiveDirectories modSpecs;
      newGhc =
        symlinkJoin
          { name = "ghci";
            paths = [ ghc ];
            postBuild =
            ''
              wrapProgram "$out/bin/ghci" \
                --add-flags "${ghciArgs}"
            '';
            buildInputs = [makeWrapper];
          };
    in
      # This symlinks the extra dirs to $PWD for GHCi to work
      writeScriptBin "ghci-with-files"
        ''
        #!/usr/bin/env bash
        set -euo pipefail

        TRAPS=""
        for i in ${lib.strings.escapeShellArgs dirs}; do
          if [ "$i" != "$PWD" ]; then
          for j in $(find "$i" ! -path "$i"); do
            file=$(basename $j)
            echo "Temporarily symlinking $j to $file..."
            ln -s $j $file
            TRAPS="rm $file ; $TRAPS"
            trap "$TRAPS" EXIT
            echo "done."
          done
          fi
        done
        ${newGhc}/bin/ghci
        '';
}
