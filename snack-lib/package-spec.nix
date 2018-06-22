{ lib
, singleOut
, callPackage
}:

with (callPackage ./modules.nix { inherit singleOut; });

rec {

  mkPackageSpec =
  packageDescr@
    { src
    , main ? null
    , ghcOpts ? []
    , dependencies ? []
    , extra-files ? []
    , extra-directories ? []
    , packages ? lib.filter (x: builtins.typeOf x != "string") dependencies
    }:
    { packageMain = main;
      packageBase = src;
      packageGhcOpts = ghcOpts;
      packageDependencies = lib.filter (x: builtins.typeOf x == "string") dependencies;

      # TODO: merge extra files and extra dirs together
      packageExtraFiles =
          if builtins.isList extra-files
          then (_x: extra-files)
          else extra-files;
      packageExtraDirectories =
            if builtins.isList extra-directories
            then (_x: extra-directories)
            else extra-directories;
      packagePackages = map mkPackageSpec packages;
    };

  flattenPackages = topPkgSpec:
    [topPkgSpec] ++ lib.lists.concatMap (flattenPackages) topPkgSpec.packagePackages;

  # TODO: nub
  allTransitiveDeps = topPkgSpec:
    lib.lists.concatMap
    (pkgSpec: pkgSpec.packageDependencies)
    (flattenPackages topPkgSpec);

  # TODO: nub
  allTransitiveGhcOpts = topPkgSpec:
    lib.lists.concatMap
    (pkgSpec: pkgSpec.packageGhcOpts)
    (flattenPackages topPkgSpec);


  # Traverses all transitive packages and returns the first package spec that
  # contains a module with given name. If none is found, returns the supplied
  # default value.
  pkgSpecByModuleName = topPkgSpec: def: modName:
    ( lib.findFirst
        (pkgSpec:
          lib.lists.elem
            modName
            (listModulesInDir pkgSpec.packageBase)
        )
        def
        (flattenPackages topPkgSpec)
    );
}
