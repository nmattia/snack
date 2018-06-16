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

let

  buildModule = ghc: ghcOpts: modSpec:
    let
      ghcOptsArgs = lib.strings.escapeShellArgs ghcOpts;
      objectName = modSpec.moduleName;
      builtDeps = map (buildModule ghc ghcOpts) modSpec.moduleImports;
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
  flattenModuleObjects = ghc: ghcOpts: mod0:
    let
      go = mod: attrs0:
        let
          objectName = x:
            # TODO: can't justuse "moduleName.o" because some modules get
            # renamed to "Main.o" :/ Also, hard coding the object file based on
            # the module name feels icky
            if x.moduleIsMain
            then "Main.o"
            else moduleToObject x.moduleName;
          attrs1 = f attrs0 mod;
          f = acc: elem:
            if lib.attrsets.hasAttr elem.moduleName acc
            then acc
            else acc //
              { "${elem.moduleName}" =
                "${buildModule ghc ghcOpts elem}/${objectName elem}";
              };
        in
          lib.lists.foldl f attrs1 mod.moduleImports;
    in go mod0 {};

  # TODO: it's sad that we pass ghcWithDeps + dependencies
  linkModuleObjects = ghc: ghcOpts: dependencies: mod:
    let
      objAttrs = flattenModuleObjects ghc ghcOpts mod;
      objList = lib.attrsets.mapAttrsToList (x: y: y) objAttrs;
      ghcOptsArgs = lib.strings.escapeShellArgs ghcOpts;
      packageList = map (p: "-package ${p}") dependencies;
    in stdenv.mkDerivation
      { name = "linker";
        src = null;
        builder = writeScript "linker-builder"
        ''
          source $stdenv/setup
          mkdir -p $out
          ${ghc}/bin/ghc \
            ${lib.strings.escapeShellArgs packageList} \
            ${lib.strings.escapeShellArgs objList} \
            ${ghcOptsArgs} \
            -o $out/out
        '';
      };

  allModuleDirectories = modSpec:
    lib.lists.concatLists
    (
    [ modSpec.moduleDirectories ]
    ++ (lib.lists.concatMap allModuleDirectories modSpec.moduleImports)
    );

  # Write a new ghci executable that loads all the modules defined in the
  # module spec
  ghciExecutable = ghc: ghcOpts: modSpec:
    let
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

  executable =
    { main
    , src
    , dependencies ? []
    , ghc-options ? []
    , extra-files ? []
    , extra-directories ? []
    }:
      let
        deps = lib.filter (x: builtins.typeOf x == "string") dependencies;
        snackDeps = lib.filter (x: builtins.typeOf x != "string" && x._type == "snack_lib_def") dependencies;
        baseByModuleName = modName:
          lib.findFirst
          (dir:
            lib.lists.elem modName (listModulesInDir dir)
            )
            base # default to base
            ((map (snackDep: snackDep.src) snackDeps) ++ [ base ] )
          ;

        ghc = haskellPackages.ghcWithPackages
          (ps: map (p: ps.${p}) deps);
        ghcOpts = ghc-options;
        base = src;
        extraFiles = if builtins.isList extra-files
          then (_x: extra-files)
          else extra-files;
        extraDirs =
            if builtins.isList extra-directories
            then (_x: extra-directories)
            else extra-directories;
        mainModName = main;
      in
    {
      build =
        linkModuleObjects
          ghc
          ghcOpts
          deps
          (makeModuleSpecRec baseByModuleName extraFiles extraDirs mainModName);
      ghci =
        ghciExecutable
          ghc
          ghcOpts
          (makeModuleSpecRec baseByModuleName extraFiles extraDirs mainModName);
    };
  library =
    { src
    , dependencies ? [] # TODO: handle this
    }:
        { _type = "snack_lib_def";
          inherit src;
          # TODO: add build for libraries
        };
in
  {
    inherit
    executable
    library
    ;
  }
