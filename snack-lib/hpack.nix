{ lib, glibcLocales, callPackage, writeText, runCommand, haskellPackages }:

with (callPackage ./lib.nix {});
with (callPackage ./modules.nix {});

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

    fromDhall = text:
      let json =

        builtins.readFile (runCommand "d2j"
        { buildInputs =
          [ (haskellPackages.ghcWithPackages (ps: [ ps.hpack-dhall ])) glibcLocales ];
        }
        "dhall-hpack-json ${text} > $out"
        );
      in builtins.fromJSON json;
in
{
  # Returns an attribute set with two fields:
  #  - library: a package spec
  #  - executable: an attr set of executable name to package spec
  pkgDescrsFromHPack = pkgHpackSrc:
    let
        package =
          let
            ext = fileExtension pkgHpackSrc;
            contents = builtins.readFile pkgHpackSrc;
          in
            if ext == null then abort "File ${pkgHpackSrc} has no extension!"
            else if ext == "yaml" then fromYAML contents
            else if ext == "yml" then fromYAML contents
            else if ext == "dhall" then fromDhall pkgHpackSrc
            else abort "File ${pkgHpackSrc} has an unknown extension (${ext})!";

        # Snack drops the version bounds because here it has no meaning
        dropVersionBounds =
          map (x: lib.lists.head (lib.strings.splitString " " x));
        mkDeps = obj: dropVersionBounds (optAttr obj "dependencies" []);
        topDeps = mkDeps package;
        topExtensions = optAttr package "default-extensions" [];
        packageLib = withAttr package "library" null (component:
            { src =
                let base = builtins.dirOf pkgHpackSrc;
                in
                  if builtins.isList component.source-dirs
                  then builtins.map (sourceDir:
                    builtins.toPath "${builtins.toString base}/${sourceDir}"
                    ) component.source-dirs
                  else
                    builtins.toPath "${builtins.toString base}/${component.source-dirs}";
              dependencies = topDeps ++ mkDeps component;
              extensions = topExtensions ++ (optAttr component "extensions" []);
            }
          );

        exes =
          withAttr package "executables" {} (lib.mapAttrs (k: v: mkExe v)) //
          withAttr package "executable" {} (comp: { ${package.name} = mkExe comp; });
        mkExe = component:
          let
            depOrPack =
              lib.lists.partition
                (x: x == package.name)
                (optAttr component "dependencies" []);
          in
            { main = fileToModule component.main;
              src =
                let base = builtins.dirOf pkgHpackSrc;
                in
                  if builtins.isList component.source-dirs
                  then builtins.map (sourceDir:
                    builtins.toPath "${builtins.toString base}/${sourceDir}"
                    ) component.source-dirs
                  else
                    builtins.toPath "${builtins.toString base}/${component.source-dirs}";
              dependencies = topDeps ++ dropVersionBounds depOrPack.wrong;
              extensions = topExtensions ++ (optAttr component "extensions" []);
            packages = map (_: packageLib) depOrPack.right;
            };
    in
      { library = packageLib;
        executables = exes;
      };
}
