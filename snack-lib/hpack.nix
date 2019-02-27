{ lib
, glibcLocales
, callPackage
, writeText
, runCommand
, haskellPackages
, pkgDescriptionsFromPath
}:

with (callPackage ./lib.nix {});
with (callPackage ./modules.nix {});
with (callPackage ./package-spec.nix {});

let
    y2j = runCommand "yaml2json"

      { buildInputs =
        [ (haskellPackages.ghcWithPackages (ps: [ ps.aeson ps.yaml ])) glibcLocales ];
      }
      "ghc ${./YamlToJson.hs} -o $out";

    fromYAML = text:
      let json =

        builtins.readFile (runCommand "y2j"
          { buildInputs = [ glibcLocales ]; }
        "${y2j} ${writeText "y2j" text}  > $out"
        );
      in builtins.fromJSON json;
in rec
{
  pkgDescriptionsFromHPack = packageYaml:
    let
        package = fromYAML (builtins.readFile packageYaml);
        base = builtins.dirOf packageYaml;
        commonAttrs = component:
          { ghcOpts = topGhcOpts ++ (optAttr component "ghc-options" []);
            extensions = topExtensions ++ (optAttr component "extensions" []);
            src =
              with { source-dirs = optAttr component "source-dirs" "."; };
              if builtins.isList source-dirs
              then builtins.map (sourceDir:
                builtins.toPath "${builtins.toString base}/${sourceDir}"
                ) source-dirs
              else
                builtins.toPath "${builtins.toString base}/${source-dirs}";
          };

        # Snack drops the version bounds because here it has no meaning
        dropVersionBounds =
          map (x: lib.lists.head (lib.strings.splitString " " x));
        mkDeps = obj: dropVersionBounds (optAttr obj "dependencies" []);
        topDeps = mkDeps package;
        topExtensions = optAttr package "default-extensions" [];
        topGhcOpts = optAttr package "ghc-options" [];
        libs = withAttr package "library" [] (component:
            [ (commonAttrs component //
                  { dependencies = topDeps ++ mkDeps component; }
              )
            ]
          );

        exes =
          withAttr package "executables" [] (lib.mapAttrsToList (k: v: mkExe k v)) ++
          withAttr package "executable" [] (comp: [(mkExe package.name comp)] );
        mkExe = nn: component:
          with
          {
            depsAndPacks = lib.foldl
              (acc: x:
                if x == package.name then tap acc "packs" (ps: ps ++ libs)
                else if lib.hasPrefix "./" x || lib.hasPrefix "/" x
                  then tap acc "packs" (ps:
                    ps ++
                    [
                      (lib.findSingle (x: ! (builtins.hasAttr "main" x))
                        (abort "Couldn't find library")
                        (abort "Found multiple libraries")
                        ( pkgDescriptionsFromPath
                          ("${builtins.toString base}/${x}")
                        )
                      )
                    ]
                  )
                else tap acc "deps" (ds: ds ++ [x])
              ) { deps = []; packs = []; } (optAttr component "dependencies" []);
          };
          commonAttrs component //
          { main = fileToModule component.main;
            name = nn;
            dependencies = topDeps ++ dropVersionBounds depsAndPacks.deps;
            packages = depsAndPacks.packs;
          };
    in exes ++ libs;
}
