# TODO: currently single out derivations prepend the PWD to the path
# TODO: make sure that filters for "base" are airtight
{ lib
, haskellPackages
, makeWrapper
, rsync
, stdenv
, symlinkJoin
, writeScript
, runCommand
, callPackage
}:

with (callPackage ./files.nix {});

# why is "inherit" needed?
with (callPackage ./modules.nix { inherit singleOut; });
with (callPackage ./module-spec.nix { inherit singleOut; });
with (callPackage ./package-spec.nix { inherit singleOut; });

let

  buildModule = ghcWith: modSpec:
    let
      ghc = ghcWith modSpec.moduleDependencies;
      ghcOpts = modSpec.moduleGhcOpts;
      ghcOptsArgs = lib.strings.escapeShellArgs ghcOpts;
      objectName = modSpec.moduleName;
      builtDeps = map (buildModule ghcWith) modSpec.moduleImports;
      depsDirs = map (x: x + "/") builtDeps;
      base = modSpec.moduleBase;
      makeSymtree =
        if lib.lists.length depsDirs >= 1
        # TODO: symlink instead of copy
        then "rsync -r ${lib.strings.escapeShellArgs depsDirs} ."
        else "";
      makeSymModule =
        # TODO: symlink instead of copy
        "rsync -r ${singleOutModule base modSpec.moduleName}/ .";
      pred = file: path: type:
        let
          topLevel = (builtins.toString base) + "/";
          actual = (lib.strings.removePrefix topLevel path);
          expected = file;
      in
        (expected == actual) ||
        (type == "directory" && (lib.strings.hasPrefix actual expected));

      extraFiles = builtins.filterSource
        (p: t:
          lib.lists.length
            (
            let
              topLevel = (builtins.toString base) + "/";
              actual = lib.strings.removePrefix topLevel p;
            in
              lib.filter (expected:
                (expected == actual) ||
                (t == "directory" && (lib.strings.hasPrefix actual expected))
                )
                modSpec.moduleFiles
            ) >= 1
        ) base;
    in stdenv.mkDerivation
    { name = objectName;
      src = symlinkJoin
        { name = "extra-files";
          paths = [ extraFiles ] ++ modSpec.moduleDirectories;
        };
      phases =
        [ "unpackPhase" "buildPhase" ];
      buildPhase =
        ''
          echo "Building module ${modSpec.moduleName}"
          mkdir -p $out
          echo "Creating dependencies symtree for module ${modSpec.moduleName}"
          ${makeSymtree}
          echo "Creating module symlink for module ${modSpec.moduleName}"
          ${makeSymModule}
          echo "Compiling module ${modSpec.moduleName}"
          # Set a tmpdir we have control over, otherwise GHC fails, not sure why
          mkdir -p tmp
          ghc -tmpdir tmp/ ${moduleToFile modSpec.moduleName} -c \
            -outputdir $out \
            ${ghcOptsArgs} \
            2>&1
          echo "Done building module ${modSpec.moduleName}"
        '';

      buildInputs =
        [ ghc
          rsync
        ];
    };


  # Returns an attribute set where the keys are the module names and the values
  # are the '.o's
  flattenModuleObjects = ghcWith: mod0:
    lib.fix (f: acc: mod:
      let objectName = x:
            # TODO: can't justuse "moduleName.o" because some modules get
            # renamed to "Main.o" :/ Also, hard coding the object file based on
            # the module name feels icky
            if x.moduleIsMain
            then "Main.o"
            else moduleToObject x.moduleName;
       in
            if lib.attrsets.hasAttr mod.moduleName acc
            then acc
            else
              let acc' = acc //
                { "${mod.moduleName}" =
                  "${buildModule ghcWith mod}/${objectName mod}";
                };
              in lib.foldl f acc' mod.moduleImports
      ) {} mod0;

  linkModuleObjects = ghcWith: mod: # main module
    let
      objAttrs = flattenModuleObjects ghcWith mod;
      objList = lib.attrsets.mapAttrsToList (x: y: y) objAttrs;
      deps = allTransitiveDeps [mod];
      ghc = ghcWith deps;
      ghcOptsArgs = lib.strings.escapeShellArgs mod.moduleGhcOpts;
      packageList = map (p: "-package ${p}") deps;
    in runCommand "linker" {}
        ''
          mkdir -p $out
          ${ghc}/bin/ghc \
            ${lib.strings.escapeShellArgs packageList} \
            ${lib.strings.escapeShellArgs objList} \
            ${ghcOptsArgs} \
            -o $out/out
        '';

  allModuleDirectories = modSpec:
    lib.lists.concatLists
    (
    [ modSpec.moduleDirectories ]
    ++ (lib.lists.concatMap allModuleDirectories modSpec.moduleImports)
    );

  # Write a new ghci executable that loads all the modules defined in the
  # module spec
  ghciExecutable = ghcWith: ghcOpts: modSpec:
    let
      ghc = ghcWith (allTransitiveDeps [modSpec]);
      ghciArgs = lib.strings.escapeShellArgs
        (ghcOpts ++ absoluteModuleFiles);
      absoluteModuleFiles =
        map
          (mod:
            builtins.toString (mod.moduleBase) +
              "/${moduleToFile mod.moduleName}"
          )
          (flattenModuleSpec modSpec);

      dirs = allModuleDirectories modSpec;
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
      writeScript "ghci-with-files"
        ''
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
  traceType = x: builtins.trace (builtins.typeOf x) x;

  executable = pkgDescr:
      let
        topPkgSpec = mkPackageSpec pkgDescr;
        baseByModuleName = modName:
          let res = pkgSpecByModuleName topPkgSpec null modName;
          in if res == null then null else res.packageBase;

        depsByModuleName = modName:
          (pkgSpecByModuleName
            topPkgSpec
            (abort "asking dependencies for external module: ${modName}")
            modName).packageDependencies
            modName
          ;
        ghcOptsByModuleName = modName:
          (pkgSpecByModuleName
            topPkgSpec
            (abort "asking ghc options for external module: ${modName}")
            modName).packageGhcOpts;

        ghcWith = deps: haskellPackages.ghcWithPackages
          (ps: map (p: ps.${p}) deps);
        base = topPkgSpec.packageBase;
        extraFiles =  topPkgSpec.packageExtraFiles;
        extraDirs = topPkgSpec.packageExtraDirectories;
        mainModName = topPkgSpec.packageMain;
        topModuleSpec =
          makeModuleSpecRec
            baseByModuleName
            extraFiles
            extraDirs
            depsByModuleName
            ghcOptsByModuleName
            mainModName;
      in
    {
      build =
        linkModuleObjects
          ghcWith
          topModuleSpec;
      ghci =
        ghciExecutable
          ghcWith
          (allTransitiveGhcOpts [topModuleSpec])
          topModuleSpec;
    };
in
  {
    inherit
    executable
    ;
  }
