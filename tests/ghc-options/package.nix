{ main = "Main";
  src = ./.;
  dependencies = [ "text" ];
  ghcOpts = ["-XOverloadedStrings"];
}
