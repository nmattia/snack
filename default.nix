let
  # Use pinned packages
  _nixpkgs = import <nixpkgs> {};
  nixpkgs = _nixpkgs.fetchFromGitHub (_nixpkgs.lib.importJSON ./nix/src.json);
  pkgs = import nixpkgs {config={}; overlays=[];};

  # Takes a (string) filepath and creates a derivation for that file (and for
  # that file only)
  singleOut = base: file:
    let
      pred = file: path: type:
        let
          topLevel = (builtins.toString base) + "/";
          actual = (pkgs.lib.strings.removePrefix topLevel path);
          expected = file;
      in expected == actual;

    in pkgs.stdenv.mkDerivation {
      name = file;
      src = builtins.filterSource (pred file) base;
      builder = pkgs.writeScript (file + "-builder")
      # TODO: make sure the file actually exists and that there's only one
      ''
        source $stdenv/setup
        mkdir -p $out
        cp -a $src/* $out/
      '';
    };

  makeModuleSpec = modName: deps: isMain:
    { moduleName = modName;
      moduleIsMain = isMain;
      moduleDependencies = deps;
    };

  moduleToFile = mod:
    (pkgs.lib.strings.replaceChars ["."] ["/"] mod) + ".hs";

  singleOutModule = base: mod: singleOut base (moduleToFile mod);

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
        ghc -tmpdir tmp/ ${mod.moduleName}.hs -c \
          -outputdir $out \
          2>&1
        echo "Done building module ${mod.moduleName}"
      '';
    buildInputs =
      [ pkgs.haskellPackages.ghc
        pkgs.rsync
      ];
    };

  # Returns an attribute set where the keys are the module names and the values
  # are the '.o's
  flattenModuleObjects = base: mod':
    let
      go = mod: attrs0:
        let
          objectName = x:
            if x.moduleIsMain
            then "Main.o"
            else x.moduleName + ".o";
          attrs1 = f attrs0 mod;
          f = acc: elem:
            if pkgs.lib.attrsets.hasAttr elem.moduleName acc
            then acc
            # TODO: module path instead of module name
            # TODO: can't use "moduleName.o" because some modules get
            # renamed to "Main.o"
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

  # TODO: use ghc -M for module dependencies
  modB = makeModuleSpec "B" [] false;
  modA = makeModuleSpec "A" [modB] true;

in linkModuleObjects ./. modA
