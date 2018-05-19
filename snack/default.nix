# TODO: currently single out derivations prepend the PWD to the path
# TODO: commented out modules (-- Foo.Module) aren't parsed properly
# TODO: make dependecies on GHC per-module if possible
# TODO: there are too many "does file exist"
# TODO: make sure that filters for "base" are airtight
# TODO: use --make everywhere ?!? NOTE: this is tricky because GHC flags
#   change: when a module is built with its dependencies, the flags for the
#   dependencies change as well, which causes them to be recompiled
{ pkgs ? import (../nix) {} # nixpkgs
}:
let
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

  makeModuleSpec = modName: deps: isMain: modFiles:
    { moduleName = modName;
      moduleIsMain = isMain;
      moduleDependencies = deps;
      moduleFiles = modFiles;
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

  # Create a module spec by following the dependencies. This assumes that the
  # specified module is a "Main" module.
  makeModuleSpecRec = base: filesByModuleName:
    pkgs.lib.fix
      (f: isMain: modName:
        makeModuleSpec
          modName
          (map (f false) (listModuleDependencies base modName))
          isMain
          (filesByModuleName modName)
      ) true;

  buildModule = ghc: base: mod:
    let
      objectName = mod.moduleName;
      builtDeps = map (buildModule ghc base) mod.moduleDependencies;
      depsDirs = map (x: x + "/") builtDeps;
      makeSymtree =
        if pkgs.lib.lists.length depsDirs >= 1
        # TODO: symlink instead of copy
        then "rsync -r ${pkgs.lib.strings.escapeShellArgs depsDirs} ."
        else "";
      makeFilesTree =
        if pkgs.lib.lists.length mod.moduleFiles >= 1
        # TODO: symlink instead of copy
        then "rsync -r ${pkgs.lib.strings.escapeShellArgs mod.moduleFiles} ."
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
        echo "Creating files symlink for module ${mod.moduleName}"
        ${makeFilesTree}
        echo "Compiling module ${mod.moduleName}"
        # Set a tmpdir we have control over, otherwise GHC fails, not sure why
        mkdir -p tmp
        ghc -tmpdir tmp/ ${moduleToFile mod.moduleName} -c \
          -outputdir $out \
          2>&1
        echo "Done building module ${mod.moduleName}"
      '';
    buildInputs =
      [ ghc
        pkgs.rsync
      ];
    };

  # Generate a list of haskell module names needed by the haskell file,
  # excluding modules that are not present in this project/base
  listModuleDependencies = base: modName:
    pkgs.lib.filter
      (doesModuleExist base)
      (builtins.fromJSON
        (builtins.readFile (listAllModuleDependenciesJSON base modName))
      );

  doesFileExist = base: filename: builtins.fromJSON (builtins.readFile
    (
    pkgs.stdenv.mkDerivation
      { name = "does-file-exist";
        src = null;

        builder = pkgs.writeScript "does-file-exist"
        ''
        if [ ! -f ${base}/${filename} ]; then
            echo -n "false" > $out
        else
            echo -n "true" > $out
        fi
        '';
      }
    ));
  doesModuleExist = base: modName: doesFileExist base (moduleToFile modName);

  # TODO: if module doesn't exist locally, don't include it
  # Lists all module dependencies, not limited to modules existing in this
  # project
  listAllModuleDependenciesJSON = base: modName:
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
  flattenModuleObjects = ghc: base: mod':
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
            if pkgs.lib.attrsets.hasAttr elem.moduleName acc
            then acc
            else acc //
              { "${elem.moduleName}" =
                "${buildModule ghc base elem}/${objectName elem}";
              };
        in
          pkgs.lib.lists.foldl f attrs1 mod.moduleDependencies;
    in go mod' {};

  # TODO: it's sad that we pass ghcWithDeps + dependencies
  linkModuleObjects = ghc: ghcOpts: dependencies: base: mod:
    let
      objAttrs = flattenModuleObjects ghc base mod;
      objList = pkgs.lib.attrsets.mapAttrsToList (x: y: y) objAttrs;
      ghcOptsArgs = pkgs.lib.strings.escapeShellArgs ghcOpts;
      packageList = map (p: "-package ${p}") dependencies;
    in pkgs.stdenv.mkDerivation
      { name = "linker";
        src = null;
        builder = pkgs.writeScript "linker-builder"
        ''
          source $stdenv/setup
          mkdir -p $out
          ${ghc}/bin/ghc \
            ${pkgs.lib.strings.escapeShellArgs packageList} \
            ${pkgs.lib.strings.escapeShellArgs objList} \
            ${ghcOptsArgs} \
            -o $out/out
        '';
      };

  # Returns a list of all module names depended on in the module spec
  allModuleNames = modSpec:
    [ modSpec.moduleName ] ++ (pkgs.lib.lists.concatMap allModuleNames modSpec.moduleDependencies);

  # Write a new ghci executable that loads all the modules defined in the
  # module spec
  ghciExecutable = ghc: ghcOpts: base: modSpec:
    let
      ghciArgs = pkgs.lib.strings.escapeShellArgs
        (ghcOpts ++ absoluteModuleFiles);
      absoluteModuleFiles = map prependBase moduleFiles;
      moduleFiles = map moduleToFile modules;
      modules = allModuleNames modSpec;
      prependBase = f: builtins.toString base + "/${f}";
    in
      pkgs.symlinkJoin
        { name = "ghci";
          paths = [ ghc ];
          postBuild =
          ''
            source $stdenv/setup
            wrapProgram "$out/bin/ghci" \
              --add-flags "${ghciArgs}"
          '';
          buildInputs = [pkgs.makeWrapper];
        };

  snack = executable:
      let
        ghc = pkgs.haskellPackages.ghcWithPackages
          (ps: map (p: ps.${p}) deps);
        deps = executable.dependencies;
        ghcOpts =
          if (builtins.hasAttr "ghc-options" executable)
          then executable.ghc-options
          else [];
        base = executable.src;
        foo =
        if (builtins.hasAttr "extra-files" executable)
        then
          if builtins.isList executable.extra-files
          then (_x: executable.extra-files)
          else executable.extra-files
        else (x: []);
        mainModName = executable.main;
      in
    {
      build =
        linkModuleObjects ghc ghcOpts deps base (makeModuleSpecRec base foo mainModName);
      ghci =
        ghciExecutable ghc ghcOpts base (makeModuleSpecRec base foo mainModName);
    };



  ## TODO: use ghc -M for module dependencies
in
  {
    inherit
    snack
    ;
  }
