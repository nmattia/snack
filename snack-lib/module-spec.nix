# Functions related to module specs
{ lib
, callPackage
, singleOut
}:

with (callPackage ./modules.nix { inherit singleOut; });

rec {
  makeModuleSpec = modName: deps: isMain: modFiles: modDirs: modBase:
    { moduleName = modName;
      moduleIsMain = isMain;
      moduleDependencies = deps;
      moduleFiles = modFiles;
      moduleDirectories = modDirs;
      moduleBase = modBase;
    };

  # Create a module spec by following the dependencies. This assumes that the
  # specified module is a "Main" module.
  makeModuleSpecRec = baseByModuleName: filesByModuleName: dirsByModuleName:
    lib.fix
      (f: isMain: modName:
        makeModuleSpec
          modName
          (map (f false)
            (listModuleDependencies baseByModuleName modName)
          )
          isMain
          (filesByModuleName modName)
          (dirsByModuleName modName)
          (baseByModuleName modName)
      ) true;

  # Returns a list of all modules in the module spec graph
  flattenModuleSpec = modSpec:
    [ modSpec ] ++
      ( lib.lists.concatMap flattenModuleSpec modSpec.moduleDependencies );

}
