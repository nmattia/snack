{ main = "Foo";
  src = ./src;
  dependencies = [
    "conduit"
    "something-that-doesnt-exist"
  ];
}
