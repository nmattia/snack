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
      moduleDependencies = modDeps;
      moduleGhcOpts = modGhcOpts;
    };

  # Create a module spec by following the dependencies. This assumes that the
  # specified module is a "Main" module.
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
            (listModuleImports baseByModuleName modName)
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

}
