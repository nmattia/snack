{ main = "Main";
  src = ./.;
  dependencies = ["file-embed"];
  extra-directories =
    (modName: if modName == "Main" then [ ../. ] else []);
}
