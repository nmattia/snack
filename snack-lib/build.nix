{ runCommand, lib, singleOut, callPackage, stdenv, rsync, symlinkJoin }:

with (callPackage ./modules.nix { inherit singleOut; });
with (callPackage ./lib.nix {});
with (callPackage ./module-spec.nix { inherit singleOut; });
rec {

  # Returns an attribute set where the keys are all the built module names and
  # the values are the paths to the object files.
  # mainModSpec: a "main" module
  buildMain = ghcWith: mainModSpec:
    buildModulesRec ghcWith
      # XXX: the main modules need special handling regarding the object name
      { "${mainModSpec.moduleName}" =
        "${buildModule ghcWith mainModSpec}/Main.o";}
      mainModSpec.moduleImports;

  # returns a attrset where the keys are the module names and the values are
  # the modules' object file path
  buildLibrary = ghcWith: modSpecs:
    buildModulesRec ghcWith {} modSpecs;

  linkMainModule = ghcWith: mod: # main module
    let
      objAttrs = buildMain ghcWith mod;
      objList = lib.attrsets.mapAttrsToList (x: y: y) objAttrs;
      deps = allTransitiveDeps [mod];
      ghc = ghcWith deps;
      ghcOptsArgs = lib.strings.escapeShellArgs mod.moduleGhcOpts;
      packageList = map (p: "-package ${p}") deps;
      relExePath = "bin/${lib.strings.toLower mod.moduleName}";
      drv = runCommand "linker" {}
        ''
          mkdir -p $out/bin
          ${ghc}/bin/ghc \
            ${lib.strings.escapeShellArgs packageList} \
            ${lib.strings.escapeShellArgs objList} \
            ${ghcOptsArgs} \
            -o $out/${relExePath}
        '';
    in
      {
        out = drv;
        relExePath = relExePath;
      };

  # Build the given modules (recursively) using the given accumulator to keep
  # track of which modules have been built already
  # XXX: doesn't work if several modules in the DAG have the same name
  buildModulesRec = ghcWith: empty: modSpecs:
    foldDAG
      { f = mod:
          { "${mod.moduleName}" =
            "${buildModule ghcWith mod}/${moduleToObject mod.moduleName}";
          };
        elemLabel = mod: mod.moduleName;
        elemChildren = mod: mod.moduleImports;
        reduce = a: b: a // b;
        empty = empty;
      }
      modSpecs;

  buildModule = ghcWith: modSpec:
    let
      ghc = ghcWith modSpec.moduleDependencies;
      exts = modSpec.moduleExtensions;
      ghcOpts = modSpec.moduleGhcOpts ++ (map (x: "-X${x}") exts);
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
}
