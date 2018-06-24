# Functions related to module specs
{ lib
, callPackage
, singleOut
}:

with (callPackage ./modules.nix { inherit singleOut; });
with (callPackage ./lib.nix {});

rec {
    makeModuleSpec =
    modName:
    modImports:
    modFiles:
    modDirs:
    modBase:
    modDeps:
    modExts:
    modGhcOpts:
    { moduleName = modName;

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
      moduleExtensions = modExts;
    };


    moduleSpecFold =
      { baseByModuleName
      , filesByModuleName
      , dirsByModuleName
      , depsByModuleName
      , extsByModuleName
      , ghcOptsByModuleName
      }:
      result:
    let
      modImportsNames = modName:
        lib.lists.filter
          (modName': ! builtins.isNull (baseByModuleName modName'))
          (listModuleImports baseByModuleName modName);
    in
      # TODO: DFS instead of Fold
      { f = modName:
          { "${modName}" =
          makeModuleSpec
            modName
            (map (mn: result.${mn}) (modImportsNames modName))
            (filesByModuleName modName)
            (dirsByModuleName modName)
            (baseByModuleName modName)
            (depsByModuleName modName)
            (extsByModuleName modName)
            (ghcOptsByModuleName modName);
          };
        empty = {} ;
        reduce = a: b: a // b;
        elemLabel = lib.id;
        elemChildren = modImportsNames;
      };

  # Returns a list of all modules in the module spec graph
  flattenModuleSpec = modSpec:
    [ modSpec ] ++
      ( lib.lists.concatMap flattenModuleSpec modSpec.moduleImports );

  allTransitiveDeps = allTransitiveLists "moduleDependencies" lib.id;
  allTransitiveGhcOpts = allTransitiveLists "moduleGhcOpts" lib.id;
  allTransitiveExtensions = allTransitiveLists "moduleExtensions" lib.id;
  allTransitiveDirectories =
    allTransitiveLists
      "moduleDirectories"
      builtins.toString; # XXX: is toString correct?
  allTransitiveImports =
    allTransitiveLists
      "moduleImports"
      (modSpec: modSpec.moduleName);

  allTransitiveLists = attr: toLabel: modSpecs:
    lib.attrsets.attrValues
    ( foldDAG
        { f = modSpec:
            lib.lists.foldl
              (x: y: x // { ${toLabel y} = y;})
              {} modSpec.${attr};
          empty = {};
          elemLabel = modSpec: modSpec.moduleName;
          reduce = a: b: a // b;
          elemChildren = modSpec: modSpec.moduleImports;
        }
        modSpecs
    );
}
