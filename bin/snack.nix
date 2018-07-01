{ runCommand, writeTextFile, symlinkJoin }:
let
  specJson = writeTextFile
    { name = "spec-json";
      text = builtins.readFile ../nix/nixpkgs/nixpkgs-src.json;
      destination = "/spec.json";
    };
  lib64 = runCommand "lib64" {}
    ''
      tar -czf lib.tar.gz -C ${../snack-lib} .
      mkdir -p $out
      base64 lib.tar.gz > $out/lib.tar.gz.b64
    '';
in
  { main = "Snack";
    src = ./.;
    dependencies =
      [
        "aeson"
        "file-embed"
        "interpolate"
        "optparse-applicative"
        "shelly"
        "text"
        "unix"
      ];
    ghcOpts = [ "-Werror" "-Wall" ] ;

    extra-directories =
      { Snack =
          [ specJson
            lib64
          ];
      };
  }
