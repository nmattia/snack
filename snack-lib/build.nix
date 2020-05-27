{ runCommand
, lib
, callPackage
, stdenv
, rsync
, symlinkJoin
, writeScript
, bash
, coreutils
}:

with (callPackage ./modules.nix {});
with (callPackage ./lib.nix {});
with (callPackage ./module-spec.nix {});

rec {

  # Returns an attribute set where the keys are all the built module names and
  # the values are the paths to the object files.
  # mainModSpec: a "main" module
  buildMain = ghcWith: mainModSpec: mainModName:
    buildModulesRec ghcWith
      # XXX: the main modules need special handling regarding the object name
      { "${mainModSpec.moduleName}" =
        "${buildModule ghcWith mainModSpec}/${mainModName}.o";}
      mainModSpec.moduleImports;

  # returns a attrset where the keys are the module names and the values are
  # the modules' object file path
  buildLibrary = ghcWith: modSpecs:
    buildModulesRec ghcWith {} modSpecs;

  linkMainModule =
      { ghcWith
      , moduleSpec # The module to build
      , name # The name to give the executable
      , mainModName
      }:
    let
      objAttrs = buildMain ghcWith moduleSpec mainModName;
      objList = lib.attrsets.mapAttrsToList (x: y: y) objAttrs;
      deps = allTransitiveDeps [moduleSpec];
      ghc = ghcWith deps;
      ghcOptsArgs = lib.strings.escapeShellArgs moduleSpec.moduleGhcOpts;
      packageList = map (p: "-package ${p}") deps;
      relExePath = "bin/${name}";
      drv = runCommand name {}
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
      ghc = ghcWith deps;
      deps = allTransitiveDeps [modSpec];
      exts = modSpec.moduleExtensions;
      ghcOpts = modSpec.moduleGhcOpts ++ (map (x: "-X${x}") exts);
      ghcOptsArgs = lib.strings.escapeShellArgs ghcOpts;
      objectName = modSpec.moduleName;
      builtDeps = map (buildModule ghcWith) (allTransitiveImports [modSpec]);
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
      src = symlinkJoin
        { name = "extra-files";
          paths = [ extraFiles ] ++ modSpec.moduleDirectories;
        };
    in builtins.derivation
    { name = objectName;
      system = stdenv.system;
      imports = map (mmm: mmm.moduleName) modSpec.moduleImports;
      PATH = lib.makeBinPath [ coreutils rsync ghc ];
      builder = writeScript "${objectName}-builder"
        ''
          #!${bash}/bin/bash
          set -e
          echo "Building module ${modSpec.moduleName}"
          echo "Local imports are:"
          for foo in $imports; do
            echo " - $foo"
          done
          cp -r ${src}/. ./

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
    };
}
