# TODO: currently single out derivations append the PWD
let
  # Use pinned packages
  pkgs = import (../nix) {};

  # Takes a (string) filepath and creates a derivation for that file (and for
  # that file only)
  singleOut = base: file:
    let
      pred = file: path: type:
        let
          topLevel = (builtins.toString base) + "/";
          actual = (pkgs.lib.strings.removePrefix topLevel path);
          expected = file;
      in
        (expected == actual) ||
        (type == "directory" && (pkgs.lib.strings.hasPrefix actual expected));
      mod = fileToModule file;

    in pkgs.stdenv.mkDerivation {
      name = mod;
      src = builtins.filterSource (pred file) base;
      builder = pkgs.writeScript (mod + "-builder")
      # TODO: make sure the file actually exists and that there's only one
      ''
        echo "Singling out module ${mod} (file is ${file})"
        source $stdenv/setup
        mkdir -p $out
        echo "Running: cp $src/${file} $out/${file}"
        echo "Listing $src"
        ls $src/**/*
        mkdir -p $(dirname $out/${file})
        cp $src/${file} $out/${file}
        echo "Done: Singling out module ${mod} (file is ${file})"
      '';
    };

  makeModuleSpec = modName: deps: isMain:
    { moduleName = modName;
      moduleIsMain = isMain;
      moduleDependencies = deps;
    };

  moduleToFile = mod:
    (pkgs.lib.strings.replaceChars ["."] ["/"] mod) + ".hs";

  moduleToObject = mod:
    (pkgs.lib.strings.replaceChars ["."] ["/"] mod) + ".o";

  fileToModule = file:
    pkgs.lib.strings.removeSuffix ".hs"
      (pkgs.lib.strings.replaceChars ["/"] ["."] file);

  singleOutModule = base: mod: singleOut base (moduleToFile mod);

  singleOutModulePath = base: mod:
    "${singleOut base (moduleToFile mod)}/${moduleToFile mod}";

  makeModuleSpecRec = base:
    pkgs.lib.fix
      (f: isMain: modName:
        makeModuleSpec
          modName
          (map (f false) (listModuleDependencies base modName))
          isMain
      ) true;

  buildFrom = base: modName: linkModuleObjects base
    (makeModuleSpecRec base modName);


  buildModule = base: mod:
    let
      objectName = mod.moduleName;
      builtDeps = map (buildModule base) mod.moduleDependencies;
      depsDirs = map (x: x + "/") builtDeps;
      makeSymtree =
        if pkgs.lib.lists.length depsDirs >= 1
        # TODO: symlink instead of copy
        then "rsync -r ${pkgs.lib.strings.escapeShellArgs depsDirs} ."
        else "";
      makeSymModule =
        # TODO: symlink instead of copy
        "rsync -r ${singleOutModule base mod.moduleName}/ .";
    in pkgs.stdenv.mkDerivation
    { name = objectName;
      src = null;
      builder = pkgs.writeScript "build-${objectName}"
      ''
        echo "Building module ${mod.moduleName}"
        source $stdenv/setup
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
          2>&1
        echo "Done building module ${mod.moduleName}"
      '';
    buildInputs =
      [ pkgs.haskellPackages.ghc
        pkgs.rsync
      ];
    };

  # Generate a list of haskell module names needed by the haskell file
  listModuleDependencies = base: modName:
    builtins.fromJSON
    (builtins.readFile (listModuleDependenciesJSON base modName));

  listModuleDependenciesJSON = base: modName:
    pkgs.stdenv.mkDerivation
      { name = "module-deps";
        src = null;
        builder = pkgs.writeScript "dependencies-json"
        ''
          echo "preparing dependencies"
          source $stdenv/setup
          # Poor man's module parser
          FILTER=$(cat <<'EOF'
          s/import\s*\(qualified\|\)\s*\(\S*\)\s*\(.*\)/\2/p;
          EOF
          )
          JSON=$(cat <<'EOF'
          s/\(.*\)/"\1"/;
          $!s/$/,/;
          EOF
          )
          sed -n "$FILTER" ${singleOutModulePath base modName} \
            | (echo "["; sed "$JSON"; echo "]") > $out
          echo "done:preparing dependencies"
          cat $out
        '';
      };

  # Returns an attribute set where the keys are the module names and the values
  # are the '.o's
  flattenModuleObjects = base: mod':
    let
      go = mod: attrs0:
        let
          objectName = x:
            # TODO: can't use "moduleName.o" because some modules get
            # renamed to "Main.o" :/
            # Also, hard coding the object file based on the module name feels
            # icky
            if x.moduleIsMain
            then "Main.o"
            else moduleToObject x.moduleName;
          attrs1 = f attrs0 mod;
          f = acc: elem:
            if pkgs.lib.attrsets.hasAttr elem.moduleName acc
            then acc
            else acc //
              { "${elem.moduleName}" =
                "${buildModule base elem}/${objectName elem}";
              };
        in
          pkgs.lib.lists.foldl f attrs1 mod.moduleDependencies;
    in go mod' {};

  linkModuleObjects = base: mod:
    let
      objAttrs = flattenModuleObjects base mod;
      objList = pkgs.lib.attrsets.mapAttrsToList (x: y: y) objAttrs;
    in pkgs.stdenv.mkDerivation
      { name = "linker";
        src = null;
        builder = pkgs.writeScript "linker-builder"
        ''
          source $stdenv/setup
          mkdir -p $out
          ghc ${pkgs.lib.strings.escapeShellArgs objList} -o $out/out
        '';
        buildInputs =
          [ pkgs.haskellPackages.ghc
          ];
      };

  ## TODO: use ghc -M for module dependencies
in
  {
    inherit
    buildFrom
    linkModuleObjects
    listModuleDependenciesJSON
    makeModuleSpec
    ;
  }
