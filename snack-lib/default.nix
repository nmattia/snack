# This is the entry point of the library, and badly needs documentation.
# TODO: currently single out derivations prepend the PWD to the path
# TODO: make sure that filters for "base" are airtight
# TODO: document the sh*t out of these functions
{ pkgs
, ghc-version ? "ghc822"
, ghcWithPackages ? pkgs.haskell.packages.${ghc-version}.ghcWithPackages
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

  # Derivation that creates a binary in a 'bin' folder.
  executable = packageFile:
    let
      specs = specsFromPackageFile packageFile;
      spec =
        if pkgs.lib.length specs == 1
        then pkgs.lib.head specs
        else abort "'executable' can only be called on a single executable";
      exe =
        if spec.packageIsExe
        then buildAsExecutable spec
        else abort "'executable' called on a library";
    in exe.out;


  # Build a package spec as resp. a library and an executable

  buildAsLibrary = pkgSpec:
    buildLibrary ghcWith (libraryModSpecs pkgSpec);

  buildAsExecutable = pkgSpec:
    let drv = linkMainModule ghcWith (executableMainModSpec pkgSpec);
    in
      { out = drv.out;
        exe_path = "${drv.out}/${drv.relExePath}";
      };

  ghcWith = deps: ghcWithPackages
    (ps: map (p: ps.${p}) deps);

  specsFromPackageFile = packageFile:
    let
      basename = builtins.baseNameOf packageFile;
      components = pkgs.lib.strings.splitString "." basename;
      ext =
        if pkgs.lib.length components <= 1
        then abort ("File " ++ packageFile ++ " does not have an extension")
        else pkgs.lib.last components;
      fromNix = [(mkPackageSpec (import packageFile))];
      fromHPack =
        let
          descrs = pkgDescrsFromHPack packageFile;
          executables =
            builtins.map mkPackageSpec descrs.executables;
          library = withAttr descrs "library" null
              (comp: if builtins.isNull comp then null else mkPackageSpec comp);
        in executables ++ (if builtins.isNull library then [] else [ library ]);
      specs =
        if ext == "nix" then fromNix
        else if ext == "yaml" then fromHPack
        else if ext == "yml" then fromHPack
        else abort ("Unknown extension " ++ ext ++ " of file " ++ packageFile);
    in specs;

  # Normal build (libs, exes)

  inferBuild = packageFile:
    mkPackages (specsFromPackageFile packageFile);

  mkPackages = pkgSpecs: writeText "build.json"
    ( builtins.toJSON
      ( builtins.map
          (pkgSpec:
            if pkgSpec.packageIsExe
            then
              { build_type = "executable";
                result = buildAsExecutable pkgSpec;
              }
            else
              { build_type = "library";
                result = buildAsLibrary pkgSpec;
              }
          ) pkgSpecs
      )
    );

  # GHCi build (libs, exes)

  inferGhci = packageFile:
    mkPackagesGhci (specsFromPackageFile packageFile);

  mkPackagesGhci = pkgSpecs: writeText "hpack-ghci-json"
    ( builtins.toJSON (
        builtins.map
          (pkgSpec:
            let
              drv =
                if pkgSpec.packageIsExe
                then ghciWithMain ghcWith (executableMainModSpec pkgSpec)
                else ghciWithModules ghcWith (libraryModSpecs pkgSpec)
                ;
            in
            { build_type = "ghci"; # TODO: need to record the name somewhere
              result = "${drv.out}/bin/ghci-with-files";
              }
          ) pkgSpecs
    ));

  # How to build resp. libraries and executables

  libraryModSpecs = pkgSpec:
    let
      moduleSpecFold' = modSpecFoldFromPackageSpec pkgSpec;
      modNames = pkgs.lib.concatMap listModulesInDir pkgSpec.packageSourceDirs;
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

in
  {
    inherit
    inferBuild
    inferGhci
    buildAsExecutable
    buildAsLibrary
    executable
    ;
  }
