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

let
  makeModuleSpec = modName: deps: isMain: modFiles: modDirs:
    { moduleName = modName;
      moduleIsMain = isMain;
      moduleDependencies = deps;
      moduleFiles = modFiles;
      moduleDirectories = modDirs;
    };

  # Create a module spec by following the dependencies. This assumes that the
  # specified module is a "Main" module.
  makeModuleSpecRec = baseByModuleName: filesByModuleName: dirsByModuleName:
    lib.fix
      (f: isMain: modName:
        makeModuleSpec
          modName
          (map (f false)
            (listModuleDependencies baseByModuleName modName)
          )
          isMain
          (filesByModuleName modName)
          (dirsByModuleName modName)
      ) true;

  buildModule = ghc: ghcOpts: baseByModuleName: mod:
    let
      ghcOptsArgs = lib.strings.escapeShellArgs ghcOpts;
      objectName = mod.moduleName;
      builtDeps = map (buildModule ghc ghcOpts baseByModuleName) mod.moduleDependencies;
      depsDirs = map (x: x + "/") builtDeps;
      base = baseByModuleName mod.moduleName;
      makeSymtree =
        if lib.lists.length depsDirs >= 1
        # TODO: symlink instead of copy
        then "rsync -r ${lib.strings.escapeShellArgs depsDirs} ."
        else "";
      makeSymModule =
        # TODO: symlink instead of copy
        "rsync -r ${singleOutModule base mod.moduleName}/ .";
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
                mod.moduleFiles
            ) >= 1
        ) base;
    in stdenv.mkDerivation
    { name = objectName;
      src = symlinkJoin
        { name = "extra-files";
          paths = [ extraFiles ] ++ mod.moduleDirectories;
        };
      phases =
        [ "unpackPhase" "buildPhase" ];
      buildPhase =
        ''
          echo "Building module ${mod.moduleName}"
          mkdir -p $out
          echo "Creating dependencies symtree for module ${mod.moduleName}"
          ${makeSymtree}
          echo "Creating module symlink for module ${mod.moduleName}"
          ${makeSymModule}
          echo "Compiling module ${mod.moduleName}"
          # Set a tmpdir we have control over, otherwise GHC fails, not sure why
          mkdir -p tmp
          ghc -tmpdir tmp/ ${moduleToFile mod.moduleName} -c \
            -outputdir $out \
            ${ghcOptsArgs} \
            2>&1
          echo "Done building module ${mod.moduleName}"
        '';

      buildInputs =
        [ ghc
          rsync
        ];
    };

  # Generate a list of haskell module names needed by the haskell file,
  # excluding modules that are not present in this project/base
  listModuleDependencies = baseByModuleName: modName:
    lib.filter
      (doesModuleExist baseByModuleName)
      (builtins.fromJSON
        (builtins.readFile (listAllModuleDependenciesJSON (baseByModuleName modName) modName))
      );

  listModulesInDir = dir: map fileToModule (listFilesInDir dir);


  doesModuleExist = baseByModuleName: modName:
    doesFileExist (baseByModuleName modName) (moduleToFile modName);


  # Lists all module dependencies, not limited to modules existing in this
  # project
  listAllModuleDependenciesJSON = base: modName:
    let
      importParser = runCommand "import-parser"
        { buildInputs =
          [ (haskellPackages.ghcWithPackages
            (ps: [ ps.haskell-src-exts ]))
          ];
        } "ghc ${./Imports.hs} -o $out" ;
    in runCommand "dependencies-json" {}
         "${importParser} ${singleOutModulePath base modName} > $out";

  # Returns an attribute set where the keys are the module names and the values
  # are the '.o's
  flattenModuleObjects = ghc: ghcOpts: baseByModuleName: mod':
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
                "${buildModule ghc ghcOpts baseByModuleName elem}/${objectName elem}";
              };
        in
          lib.lists.foldl f attrs1 mod.moduleDependencies;
    in go mod' {};

  # TODO: it's sad that we pass ghcWithDeps + dependencies
  linkModuleObjects = ghc: ghcOpts: dependencies: baseByModuleName: mod:
    let
      objAttrs = flattenModuleObjects ghc ghcOpts baseByModuleName mod;
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

  # Returns a list of all module names depended on in the module spec
  allModuleNames = modSpec:
    [ modSpec.moduleName ] ++ (lib.lists.concatMap allModuleNames modSpec.moduleDependencies);

  allModuleDirectories = modSpec:
    lib.lists.concatLists
    (
    [ modSpec.moduleDirectories ]
    ++ (lib.lists.concatMap allModuleDirectories modSpec.moduleDependencies)
    );

  # Write a new ghci executable that loads all the modules defined in the
  # module spec
  ghciExecutable = ghc: ghcOpts: baseByModuleName: modSpec:
    let
      ghciArgs = lib.strings.escapeShellArgs
        (ghcOpts ++ absoluteModuleFiles);
      absoluteModuleFiles =
        map
          (modName:
            builtins.toString (baseByModuleName modName) + "/${moduleToFile modName}"
          )
          modules;

      #absoluteModuleFiles = map prependBase moduleFiles;
      #moduleFiles = map moduleToFile modules;
      modules = allModuleNames modSpec;
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
          baseByModuleName
          (makeModuleSpecRec baseByModuleName extraFiles extraDirs mainModName);
      ghci =
        ghciExecutable
          ghc
          ghcOpts
          baseByModuleName
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
