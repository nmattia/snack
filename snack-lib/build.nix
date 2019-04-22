{ runCommand
, lib
, callPackage
, stdenv
, rsync
, symlinkJoin
}:

with (callPackage ./modules.nix {});
with (callPackage ./lib.nix {});
with (callPackage ./module-spec.nix {});

rec {

  linkMainModule =
      { ghcWith
      , moduleSpec # The module to build
      , name # The name to give the executable
      }:
    let
      buildPlan' = buildPlan ghcWith
        # XXX: the main modules need special handling regarding the object name
        (modName:
          if modName == moduleSpec.moduleName
          then "Main.o"
          else "${moduleToObject modName}"
        ) [moduleSpec];
      objAttrs = builtins.mapAttrs (k: v: v.built) buildPlan';
      objList = lib.attrsets.mapAttrsToList (x: y: y) objAttrs;
      deps = buildPlan'.${moduleSpec.moduleName}.transitive.moduleDependencies;
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

  # returns a attrset where the keys are the module names and the values are
  # the modules' object file path
  buildLibrary = ghcWith: modSpecs:
    buildModulesRec ghcWith
      (modName: "${moduleToObject modName}")
    modSpecs;

  # TODO: what is this?
  buildPlan = ghcWith: objectName: modSpecs:
    with rec
      { getTransitiveAttr = attr: modSpec:
          lib.unique (
            modSpec.${attr} ++
              lib.concatLists (map (modSpec': finalSet.${modSpec'.moduleName}.transitive.${attr}) modSpec.moduleImports)
            );

        getBuilts = modSpec:
          lib.foldl
            (acc: modSpec': acc // { "${modSpec'.moduleName}" = buildModule ghcWith finalSet.${modSpec'.moduleName}.transitive getBuilts modSpec'; })
            {} modSpec.moduleImports //
          lib.foldl (acc: modSpec': acc // getBuilts modSpec') {} modSpec.moduleImports;

        wrap = modSpec: rec
          { key = modSpec.moduleName;
            inherit modSpec;
            built = "${buildModule ghcWith transitive getBuilts modSpec}/${objectName modSpec.moduleName}";
            transitive =
              { moduleDependencies = getTransitiveAttr "moduleDependencies" modSpec;
                moduleGhcOpts = getTransitiveAttr "moduleGhcOpts" modSpec;
                moduleExtensions = getTransitiveAttr "moduleExtensions" modSpec;
                moduleImports = getTransitiveAttr "moduleImports" modSpec;
                moduleDirectories = getTransitiveAttr "moduleDirectories" modSpec;
              };
            builts = getBuilts modSpec;
          };

        toSet = objs:
          lib.listToAttrs (
            map (obj: { name = obj.modSpec.moduleName; value = obj; })
            objs
            );

        finalSet = toSet finalList;

        finalList =
          builtins.genericClosure
            { startSet = map wrap modSpecs;
              operator = obj: map wrap obj.modSpec.moduleImports;
            };

        allBuiltModules = finish finalList;
      };
    finalSet;

  # Build the given modules recursively.
  # XXX: doesn't work if several modules in the DAG have the same name
  buildModulesRec = ghcWith: objectName: modSpecs:
    lib.mapAttrs (k: v: v.built) (buildPlan ghcWith objectName modSpecs);

  buildModule = ghcWith: transitive: getBuilts: modSpec:
    let
      ghc = ghcWith deps;
      deps = transitive.moduleDependencies;
      exts = modSpec.moduleExtensions;
      ghcOpts = modSpec.moduleGhcOpts ++ (map (x: "-X${x}") exts);
      ghcOptsArgs = lib.strings.escapeShellArgs ghcOpts;
      objectName = modSpec.moduleName;
      builtDeps = builtins.attrValues (getBuilts modSpec);
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

      imports = map (mmm: mmm.moduleName) modSpec.moduleImports;
      buildPhase =
        ''
          echo "Building module ${modSpec.moduleName}"
          echo "Local imports are:"
          for foo in $imports; do
            echo " - $foo"
          done

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

          ls $out
          echo "Done building module ${modSpec.moduleName}"
        '';

      buildInputs =
        [ ghc
          rsync
        ];
    };
}
