{ main = "Main";
  src = ./src;
  dependencies = ["file-embed"];
  extra-directories =
    (modName: if modName == "Main" then [ ./assets ] else []);
}
