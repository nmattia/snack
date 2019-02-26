{ lib, glibcLocales, callPackage, writeText, runCommand, haskellPackages }:

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
in
{
  # Returns an attribute set with two fields:
  #  - library: a package spec
  #  - executable: an attr set of executable name to package spec
  pkgSpecsFromHPack = packageYaml:
    let
        package = fromYAML (builtins.readFile packageYaml);

        # Snack drops the version bounds because here it has no meaning
        dropVersionBounds =
          map (x: lib.lists.head (lib.strings.splitString " " x));
        mkDeps = obj: dropVersionBounds (optAttr obj "dependencies" []);
        topDeps = mkDeps package;
        topExtensions = optAttr package "default-extensions" [];
        topGhcOpts = optAttr package "ghc-options" [];
        libs = withAttr package "library" [] (component:
            [{
              src =
                let
                  base = builtins.dirOf packageYaml;
                  source-dirs = optAttr component "source-dirs" ".";
                in
                  if builtins.isList source-dirs
                  then builtins.map (sourceDir:
                    builtins.toPath "${builtins.toString base}/${sourceDir}"
                    ) source-dirs
                  else
                    builtins.toPath "${builtins.toString base}/${source-dirs}";
              dependencies = topDeps ++ mkDeps component;
              extensions = topExtensions ++ (optAttr component "extensions" []);
              ghcOpts = topGhcOpts ++ (optAttr component "ghc-options" []);
            }]
          );

        exes =
          withAttr package "executables" [] (lib.mapAttrsToList (k: v: mkExe k v)) ++
          withAttr package "executable" [] (comp: [(mkExe package.name comp)] );
        mkExe = nn: component:
          let
            depOrPack =
              lib.lists.partition
                (x: x == package.name)
                (optAttr component "dependencies" []);
          in
            { main = fileToModule component.main;
              name = nn;
              src =
                let
                  base = builtins.dirOf packageYaml;
                  source-dirs = optAttr component "source-dirs" ".";
                in
                  if builtins.isList source-dirs
                  then builtins.map (sourceDir:
                    builtins.toPath "${builtins.toString base}/${sourceDir}"
                    ) source-dirs
                  else
                    builtins.toPath "${builtins.toString base}/${source-dirs}";
              dependencies = topDeps ++ dropVersionBounds depOrPack.wrong;
              extensions = topExtensions ++ (optAttr component "extensions" []);
              ghcOpts = topGhcOpts ++ (optAttr component "ghc-options" []);
              packages = if lib.length depOrPack.right > 0 then libs else [];
            };
    in exes ++ libs;
}
