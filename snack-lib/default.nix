# This is the entry point of the library, and badly needs documentation.
# TODO: currently single out derivations prepend the PWD to the path
# TODO: make sure that filters for "base" are airtight
# TODO: document the sh*t out of these functions
{ pkgs
, ghc-version ? "ghc864"
, ghcWithPackages ? pkgs.haskell.packages.${ghc-version}.ghcWithPackages
, haskellPackages ? pkgs.haskell.packages.${ghc-version}
}:

with pkgs;

with (callPackage ./build.nix {});
with (callPackage ./files.nix {});
with (callPackage ./ghci.nix {});
with (callPackage ./lib.nix {});
with (callPackage ./modules.nix {});
with (callPackage ./module-spec.nix {});
with (callPackage ./package-spec.nix {});

with rec
{
  hpack = callPackage ./hpack.nix { inherit pkgDescriptionsFromPath; };

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
    let
      moduleSpec = executableMainModSpec pkgSpec;
      name = pkgSpec.packageName;
      mainModName = pkgSpec.packageMainModule;
      drv = linkMainModule { inherit moduleSpec name ghcWith mainModName; };
    in
      { out = drv.out;
        exe_path = "${drv.out}/${drv.relExePath}";
      };

  ghcWith = deps: ghcWithPackages
    (ps: map (p: ps.${p}) deps);

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

  # Get a list of package descriptions from a path
  # This can be
  #  - a path, relative or absolute, to a directory that contains either a
  #     package.yaml or a package.nix
  #  - a path, relative or absolute, to a file with either .nix or .yaml or
  #     .yml extension

  pkgDescriptionsFromPath =
    with rec
    {
      pkgDescriptionsFromFile = packageFile:
        with rec
        {
          basename = builtins.baseNameOf packageFile;
          components = pkgs.lib.strings.splitString "." basename;
          ext =
            if pkgs.lib.length components <= 1
            then abort ("File " ++ packageFile ++ " does not have an extension")
            else pkgs.lib.last components;
          fromNix = [(import packageFile)];
          fromHPack = hpack.pkgDescriptionsFromHPack packageFile;
        };
        if ext == "nix" then fromNix
        else if ext == "yaml" then fromHPack
        else if ext == "yml" then fromHPack
        else abort ("Unknown extension " ++ ext ++ " of file " ++ packagePath);
      pkgDescriptionsFromDir = packageDir:
        with rec
        { dirContent = builtins.readDir packageDir;
          hasPackageYaml = builtins.hasAttr "package.yaml" dirContent;
          hasPackageNix = builtins.hasAttr "package.nix" dirContent;
        };
        if hasPackageYaml && hasPackageNix
          then abort "Found both package.yaml and package.nix in ${packageDir}"
        else if ! (hasPackageYaml || hasPackageNix)
          then abort "Couldn't find package.yaml or package.nix in ${packageDir}"
        else if hasPackageYaml
          then pkgDescriptionsFromFile
            "${builtins.toString packageDir}/package.yaml"
        else  pkgDescriptionsFromFile
            "${builtins.toString packageDir}/package.nix";
    };

    packagePath:
    with { pathType = pkgs.lib.pathType packagePath ; } ;
    if pathType == "directory"
      then pkgDescriptionsFromDir packagePath
    else if pathType == "regular"
      then pkgDescriptionsFromFile packagePath
    else abort "Don't know how to load package path of type ${pathType}";

  specsFromPackageFile = packagePath:
    map mkPackageSpec (pkgDescriptionsFromPath packagePath);

  buildHoogle = packagePath:
    let
      concatUnion = lists:
        let
          sets = map (l: pkgs.lib.genAttrs l (_: null)) lists;
          union = pkgs.lib.foldAttrs (n: a: null) {} sets;
        in
          builtins.attrNames union;
      allDeps = concatUnion (map (spec: spec.packageDependencies {}) (specsFromPackageFile packagePath));
      drv = haskellPackages.hoogleLocal { packages = map (p: haskellPackages.${p}) allDeps; };
    in
    writeText "hoogle-json"
    ( builtins.toJSON
        { build_type = "hoogle";
          result = {
            exe_path = "${drv.out}/bin/hoogle";
          };
        }
    );

  buildHieBios = packagePath: haskellPath:
    let
      flags = with lib; let
        containingPackage = let
          hasSource = let
            isPrefix = xxs: xs: length xs == 0 || length xxs > 0 && head xs == head xxs && isPrefix (tail xxs) (tail xs);
            components = p: strings.splitString "/" (builtins.toString p); # toString needed because otherwise the coercion of the path to a string will import it into the store first
          in spec: any (isPrefix (components haskellPath)) (map components spec.packageSourceDirs) || any hasSource spec.packagePackages;
        in findFirst hasSource (abort "Couldn't find package containing ${builtins.toString haskellPath} in ${builtins.toString packagePath} and the packages it references") (specsFromPackageFile packagePath);
        srcdirs = let
          sourceDirs = spec: spec.packageSourceDirs ++ (map sourceDirs spec.packagePackages);
        in map builtins.toString (unique (sourceDirs containingPackage));
        modspecs = if containingPackage.packageIsExe then [ (executableMainModSpec containingPackage) ] else libraryModSpecs containingPackage;
        deps = allTransitiveDeps modspecs;
        exts = allTransitiveExtensions modspecs;
        opts = allTransitiveGhcOpts modspecs;
        db = let
          ghc = ghcWith deps;
        in "${ghc}/lib/ghc-${ghc.version}/package.conf.d";
      in [ "-no-global-package-db" "-package-db ${db}" ] ++ (map (p: "-package ${p}") deps) ++ (map (d: "-i${d}") srcdirs) ++ (map (e: "-X${e}") exts) ++ opts;
    in
      writeText "hie-bios-json" (
        builtins.toJSON {
          build_type = "hie-bios";
          result = {
            hie-bios_flags = flags;
          };
        }
      );

};
{
  inherit
  inferBuild
  inferGhci
  buildAsExecutable
  buildAsLibrary
  executable
  buildHoogle
  buildHieBios
  ;
}
