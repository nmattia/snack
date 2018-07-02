{ lib, glibcLocales, callPackage, singleOut, writeText, runCommand, haskellPackages }:

with (callPackage ./modules.nix { inherit singleOut; });

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
  snackNixFromHPack = packageYaml:
    let
        package = fromYAML (builtins.readFile packageYaml);
        topDeps =
          # this drops the version bounds
          map (x: lib.lists.head (lib.strings.splitString " " x))
          package.dependencies;
        extensions = package.default-extensions;
        packageLib =
          let component = package.library;
          in
            { src =
                let base = builtins.dirOf packageYaml;
                in builtins.toPath "${builtins.toString base}/${component.source-dirs}";
              dependencies = topDeps ++
                (if builtins.hasAttr "dependencies" component
                then component.dependencies
                else []);

              inherit extensions;
            };

        exes =
          if builtins.hasAttr "executables" package
          then lib.mapAttrs (k: v: mkExe v) package.executables
          else {};
        mkExe = component:
          let
            depOrPack =
              lib.lists.partition
                (x: x == package.name)
                (if builtins.hasAttr "dependencies" component
                then component.dependencies
                else []);
            packages = map (_: packageLib) depOrPack.right;
            dependencies = topDeps ++ depOrPack.wrong;
          in
            { main = fileToModule component.main;
              src =
                let
                  base = builtins.dirOf packageYaml;
                in builtins.toPath "${builtins.toString base}/${component.source-dirs}";
              inherit packages dependencies extensions;
            };
    in
      { library = packageLib;
        executables = exes;
      };
}
