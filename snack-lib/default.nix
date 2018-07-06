# TODO: currently single out derivations prepend the PWD to the path
# TODO: make sure that filters for "base" are airtight
{ lib
, haskellPackages
, makeWrapper
, rsync
, stdenv
, symlinkJoin
, writeScriptBin
, writeText
, runCommand
, callPackage
}:

with (callPackage ./files.nix {});

# why is "inherit" needed?
with (callPackage ./modules.nix { inherit singleOut; });
with (callPackage ./module-spec.nix { inherit singleOut; });
with (callPackage ./package-spec.nix { inherit singleOut; });
with (callPackage ./hpack.nix { inherit singleOut; });
with (callPackage ./lib.nix {});
with (callPackage ./build.nix { inherit singleOut; });
with (callPackage ./ghci.nix { inherit singleOut; });

let

  # TODO: "executable" is a bad name
  executable = pkgDescr:
    let
      moduleSpecFold' = modSpecFoldFromPackageSpec topPkgSpec;
      topPkgSpec = mkPackageSpec pkgDescr;
      ghcWith = deps: haskellPackages.ghcWithPackages
        (ps: map (p: ps.${p}) deps);
    in
      if builtins.isNull topPkgSpec.packageMain
      then
        let
          modNames = listModulesInDir topPkgSpec.packageBase;
          fld = moduleSpecFold' modSpecs';
          modSpecs' = foldDAG fld modNames;
          modSpecs = builtins.attrValues modSpecs';
        in
          {
            build =
              # This json is a bit different than the other ones, because it's
              # a map of modules to object files (rather than out_path +
              # exe_path)
              { json = writeText "build_output"
                  (builtins.toJSON (buildLibrary ghcWith modSpecs));
              };
            ghci =
              let
                drv = ghciWithModules ghcWith modSpecs;
                json =
                  { out_path = "${drv.out}";
                    exe_path = "${drv.out}/bin/ghci-with-files";
                  };
              in
                { out = drv.out;
                  json = writeText "ghci_output" (builtins.toJSON json);
                };
          }
      else
        let
          mainModName = topPkgSpec.packageMain;
          mainModSpec =
            let
              fld = moduleSpecFold' modSpecs;
              modSpecs = foldDAG fld [mainModName];
            in modSpecs.${mainModName};
        in
          { build =
              let
                drv = linkMainModule ghcWith mainModSpec;
                json =
                  { out_path = "${drv.out}";
                    exe_path = "${drv.out}/${drv.relExePath}";
                  };
              in
                { out = drv.out;
                  json = writeText "build_output" (builtins.toJSON json);
                };

            ghci =
              let
                drv = ghciWithMain ghcWith mainModSpec;
                json =
                  { out_path = "${drv.out}";
                    exe_path = "${drv.out}/bin/ghci-with-files";
                  };
                in
                  { out = drv.out;
                    json = writeText "ghci_output" (builtins.toJSON json);
                  };
          };
      packageYaml = pyam:
        let
          project = snackNixFromHPack pyam;
        in
          { library = executable project.library;
            executables = lib.attrsets.mapAttrs (_: v: executable v) project.executables;
          };
in
  {
    inherit
    executable
    packageYaml
    ;
  }
