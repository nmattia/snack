let
  my-lib =
    { src = ./src;
      dependencies = [ "conduit" ];
    };
in
  { src = ./lib;
    dependencies = [ "conduit" ];
    packages = [ my-lib ];
  }
