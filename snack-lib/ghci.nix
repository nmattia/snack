{ makeWrapper, symlinkJoin, lib, callPackage, writeScriptBin }:

with (callPackage ./module-spec.nix {});
with (callPackage ./modules.nix {});
with (callPackage ./build.nix {});

rec {

  # Write a new ghci executable that loads all the modules defined in the
  # module spec
  ghciWithMain = ghcWith: mainModSpec:
    let
      buildPlan' = buildPlan ghcWith
        (modName: abort "GHCi shouldn't need objects") [mainModSpec];
      imports = buildPlan'.${mainModSpec.moduleName}.transitive.moduleImports;
      modSpecs = [mainModSpec] ++ imports;
    in ghciWithModules ghcWith modSpecs;

  ghciWithModules = ghcWith: modSpecs:
    let
      buildPlan' = buildPlan ghciWith
        (modName: abort "GHCi shouldn't need objects") modSpecs;
      allAttr = attr: lib.unique (lib.concatMap (modSpec: modSpec.${attr}) modSpecs);
      allGhcOpts = allAttr "moduleGhcOpts";
      allExtensions = allAttr "moduleExtensions";
      allDependencies = allAttr "moduleDependencies";
      ghcOpts = allGhcOpts ++ (map (x: "-X${x}") allExtensions);
      ghc = ghcWith allDependencies;
      dirs = allAttr "moduleDirectories";
      ghciArgs = ghcOpts ++ absoluteModuleFiles;
      absoluteModuleFiles =
        map
          (mod:
            builtins.toString (mod.moduleBase) +
              "/${moduleToFile mod.moduleName}"
          )
          modSpecs;
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
        ${ghc}/bin/ghci ${lib.strings.escapeShellArgs ghciArgs}
        '';
}
