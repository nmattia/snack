# This is the entry point of the library, and badly needs documentation.
# TODO: currently single out derivations prepend the PWD to the path
# TODO: make sure that filters for "base" are airtight
{ pkgs
}:

with pkgs;

with (callPackage ./build.nix {});
with (callPackage ./files.nix {});
with (callPackage ./ghci.nix {});
with (callPackage ./lib.nix {});
with (callPackage ./modules.nix {});
with (callPackage ./module-spec.nix {});
with (callPackage ./package-spec.nix {});
with (callPackage ./hpack.nix {});

let
  ghcWith = deps: haskellPackages.ghcWithPackages
    (ps: map (p: ps.${p}) deps);

  # Assumes the package description describes an executable
  withMainModSpec = pkgDescr: act:
    let
      mainModName = pkgDescr.packageMain;
      mainModSpec =
        let
          fld = moduleSpecFold' modSpecs;
          modSpecs = foldDAG fld [mainModName];
        in modSpecs.${mainModName};
      drv = linkMainModule ghcWith mainModSpec;
    in
      { out = drv.out;
        outPath = "${drv.out}";
        exePath = "${drv.out}/${drv.relExePath}";
      };

  libraryModSpecs = pkgSpec:
    let
      moduleSpecFold' = modSpecFoldFromPackageSpec pkgSpec;
      modNames = listModulesInDir pkgSpec.packageBase;
      fld = moduleSpecFold' modSpecs';
      modSpecs' = foldDAG fld modNames;
      modSpecs = builtins.attrValues modSpecs';
    in modSpecs;

  executableMainModSpec = pkgSpec:
    let
      moduleSpecFold' = modSpecFoldFromPackageSpec pkgSpec;
      mainModName = pkgSpec.packageMain;
      mainModSpec =
        let
          fld = moduleSpecFold' modSpecs;
          modSpecs = foldDAG fld [mainModName];
        in modSpecs.${mainModName};
    in mainModSpec;

  buildAsLibrary = pkgSpec:
    buildLibrary ghcWith (libraryModSpecs pkgSpec);

  buildAsExecutable = pkgSpec:
    let drv = linkMainModule ghcWith (executableMainModSpec pkgSpec);
    in
      { out = drv.out;
        exe_path = "${drv.out}/${drv.relExePath}";
      };

  inferSnackBuild = packageNix: mkPackage (import packageNix);

  inferSnackGhci = packageNix: writeText "snack-ghci-json"
    ( builtins.toJSON (
    let
      pkgSpec = mkPackageSpec (import packageNix);
      drv =
        if builtins.isNull pkgSpec.packageMain
        then ghciWithModules ghcWith (libraryModSpecs pkgSpec)
        else ghciWithMain ghcWith (executableMainModSpec pkgSpec);
    in
      { build_type = "ghci";
        result = {
            "ghci_path" = "${drv.out}/bin/ghci-with-files";
          };
      }
    ));

  inferHPackBuild = packageYaml: writeText "hpack-build-json"
    ( builtins.toJSON (
    let pkgSpecs = hpackSpecs packageYaml;
    in
      { build_type = "multi";
        result =
          { library =
              if builtins.isNull pkgSpecs.library
              then null
              else buildAsLibrary (pkgSpecs.library);
            executables = lib.attrsets.mapAttrs (k: v: buildAsExecutable v) pkgSpecs.executables;
          };
      }
    ));

  inferHPackGhci = packageYaml: writeText "hpack-ghci-json"
    ( builtins.toJSON (
    let
      pkgSpecs = hpackSpecs packageYaml;
      pkgSpec = mkPackageSpec (import packageNix);
      drv =
        let exeSpecs = builtins.attrValues pkgSpecs.executables;
        in
          if lib.lists.length exeSpecs == 1
          then ghciWithMain ghcWith (executableMainModSpec (lib.lists.head exeSpecs))
          else
            if builtins.isNull pkgSpecs.library
            then abort "GHCi: needs either a single executable or a library"
            else ghciWithModules ghcWith (libraryModSpecs pkgSpecs.library);
    in
      { build_type = "ghci";
        result = {
            "ghci_path" = "${drv.out}/bin/ghci-with-files";
          };
      }
    ));

  snackSpec = packageNix: mkPackageSpec (import packageNix);
  hpackSpecs = packageYaml:
    let
      descrs = pkgDescrsFromHPack packageYaml;
    in
      { library = withAttr descrs "library" null
          (comp: if builtins.isNull comp then null else mkPackageSpec comp);
        executables =
          lib.attrsets.mapAttrs (k: v: mkPackageSpec v) descrs.executables;
      };

  mkPackage = snackNixExpr: writeText "snack-build-json"
    ( builtins.toJSON (
    let
      pkgSpec = mkPackageSpec snackNixExpr;
    in
      if builtins.isNull pkgSpec.packageMain
      then
        { "build_type" = "library";
          "result" = buildAsLibrary pkgSpec;
        }
      else
        { "build_type" = "executable";
          "result" = buildAsExecutable pkgSpec;
        }
    ));

in
  {
    inherit
    inferSnackBuild
    inferSnackGhci
    inferHPackBuild
    inferHPackGhci
    packageYaml
    buildAsExecutable
    buildAsLibrary
    snackSpec
    hpackSpec
    mkPackage
    ;
  }
