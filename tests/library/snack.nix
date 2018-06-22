let
  my-lib =
    { src = ./src;
      dependencies = [ "conduit" ];
    };
in
  { main = "Foo";
    src = ./app;
    dependencies = [ my-lib "conduit" ];
  }
