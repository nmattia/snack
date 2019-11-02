let
  pkgs = import ../nix {};
  specJson = pkgs.writeTextFile
    { name = "spec-json";
      text = builtins.toJSON { inherit (pkgs.sources.nixpkgs) sha256 url; } ;
      destination = "/spec.json";
    };
  lib64 = pkgs.runCommand "lib64" {}
    ''
      tar -czf lib.tar.gz -C ${pkgs.lib.cleanSource ../snack-lib} .
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
        "unliftio"
      ];
    ghcOpts = [ "-Werror" "-Wall" ] ;

    extra-directories =
      { Snack =
          [ specJson
            lib64
          ];
      };
  }
