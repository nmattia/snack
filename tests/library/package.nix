let
  my-lib =
    { src = ./src;
      dependencies = [ "conduit" ];
    };
in
  { main = "Foo";
    src = ./app;
    dependencies = [ "conduit" ];
    packages = [ my-lib ];
  }
