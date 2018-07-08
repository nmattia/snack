{ lib
, callPackage
}:

with (callPackage ./modules.nix {});

rec {

  mkPackageSpec =
  packageDescr@
    { src
    , main ? null
    , ghcOpts ? []
    , dependencies ? []
    , extensions ? []
    , extra-files ? []
    , extra-directories ? []
    , packages ? []
    }:
    { packageMain = main;
      packageBase = src;
      packageGhcOpts = ghcOpts;
      packageExtensions = extensions;
      packageDependencies = mkPerModuleAttr dependencies;

      # TODO: merge extra files and extra dirs together
      packageExtraFiles = mkPerModuleAttr extra-files;
      packageExtraDirectories = mkPerModuleAttr extra-directories;
      packagePackages = map mkPackageSpec packages;
    };

  mkPerModuleAttr = attr:
    if builtins.isList attr
    then (_: attr)
    else if builtins.isAttrs attr
    then (x: if builtins.hasAttr x attr then attr.${x} else [])
    else if builtins.isFunction attr
    then attr
    else
      abort "Unknown type for per module attributes: ${builtins.typeOf attr}";

  flattenPackages = topPkgSpec:
    [topPkgSpec] ++ lib.lists.concatMap (flattenPackages) topPkgSpec.packagePackages;

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
