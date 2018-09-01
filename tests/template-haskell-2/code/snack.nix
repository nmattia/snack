{ main = "Main";
  src = ./.;
  dependencies = ["file-embed"];
  extra-directories =
   { Main = [../.]; };
}
