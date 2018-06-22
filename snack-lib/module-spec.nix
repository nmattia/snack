# Functions related to module specs
{ lib
, callPackage
, singleOut
}:

with (callPackage ./modules.nix { inherit singleOut; });

rec {
    makeModuleSpec =
    modName:
    modImports:
    isMain:
    modFiles:
    modDirs:
    modBase:
    modDeps:
    modGhcOpts:
    { moduleName = modName;
      moduleIsMain = isMain;

      # local module imports, i.e. not part of an external dependency
      moduleImports = modImports;

      moduleFiles = modFiles;
      moduleDirectories = modDirs;
      moduleBase = modBase;
      moduleDependencies =
        if builtins.isList modDeps
        then modDeps
        else abort "module dependencies should be a list";
      moduleGhcOpts = modGhcOpts;
    };

  # Create a module spec by following the dependencies. This assumes that the
  # specified module is a "Main" module.
  # TODO: pretty sure things will silently go wrong if several modules in the
  # dependency tree share a common name
    makeModuleSpecRec =
    baseByModuleName:
    filesByModuleName:
    dirsByModuleName:
    depsByModuleName:
    ghcOptsByModuleName:
    lib.fix
      (f: isMain: modName:
        makeModuleSpec
          modName
          (map (f false)
            (lib.lists.filter (mn: baseByModuleName mn != null) (listModuleImports baseByModuleName modName))
          )
          isMain
          (filesByModuleName modName)
          (dirsByModuleName modName)
          (baseByModuleName modName)
          (depsByModuleName modName)
          (ghcOptsByModuleName modName)
      ) true;

  # Returns a list of all modules in the module spec graph
  flattenModuleSpec = modSpec:
    [ modSpec ] ++
      ( lib.lists.concatMap flattenModuleSpec modSpec.moduleImports );

  allTransitiveDeps = allTransitiveLists "moduleDependencies";
  allTransitiveGhcOpts = allTransitiveLists "moduleGhcOpts";

  allTransitiveLists = attr: modSpecs:
    lib.attrsets.attrNames
    (
    lib.fix
      (f: mods: deps: modSpecs:
        if lib.lists.length modSpecs == 0
        then deps
        else
          let
            modSpec = lib.lists.head modSpecs;
            modSpecs' = lib.lists.tail modSpecs;
            newDeps = lib.attrsets.listToAttrs
              (map (dep: { name = dep; value = null; })
                modSpec.${attr});
            deps' = deps // newDeps;
            mods' = mods // { ${modSpec.moduleName} = null; };
          in f mods' deps' modSpecs'
        ) {} {} modSpecs
      )  ;

}
