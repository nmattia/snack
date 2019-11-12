{ lib
, callPackage
}:

with (callPackage ./modules.nix {});

rec {

  mkPackageSpec =
  packageDescr@
    { src ? []
    , name ? null
    , main ? null
    , mainModule ? "Main"
    , ghcOpts ? []
    , dependencies ? []
    , extensions ? []
    , extra-files ? []
    , extra-directories ? []
    , packages ? []
    }:
    with
    rec {
      isExe = ! builtins.isNull main;
      pName =
        if isExe && builtins.isNull name
        then lib.strings.toLower main
        else name;
    };
    { packageIsExe = ! builtins.isNull main;
      packageName = pName;
      packageMain = main;
      packageMainModule = mainModule;
      packageSourceDirs =
        if builtins.isList src
        then src
        else [src];
      packageGhcOpts = let
        addition =
          if mainModule == "Main"
          then
            []
          else
            ["-main-is" mainModule];
        in
          ghcOpts ++ addition;
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
  pkgSpecAndBaseByModuleName = topPkgSpec: modName:
    let
      foo = pkgSpec:
        lib.findFirst
          (base: lib.lists.elem modName (listModulesInDir base))
          null
          pkgSpec.packageSourceDirs;
      bar = lib.concatMap
        (pkgSpec:
          let base = foo pkgSpec;
          in if base == null then [] else [ { inherit pkgSpec base; } ])
        (flattenPackages topPkgSpec);
    in if lib.length bar <= 0 then null else
       if lib.length bar == 1 then lib.head bar
       else abort
        "Refusing to return base, module name was found more than once: ${modName}";

  pkgSpecByModuleName = topPkgSpec: def: modName:
    let
      res = pkgSpecAndBaseByModuleName topPkgSpec modName;
    in if res == null then def else res.pkgSpec;
}
