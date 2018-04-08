# This "snack" passing is ugly, figure out a nice way of passing snack-lib
with (import ../../nix {}).snack-lib-with
  { dependencies = ["conduit"] ;
  };
buildFrom {
  name = "Foo";
  src = ./src;
}
