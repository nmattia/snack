# This "snack" passing is ugly, figure out a nice way of passing snack-lib
with (import ../../nix {}).snack-lib;
let
  modB = makeModuleSpec "Foo.B.C" [] false;
  modA = makeModuleSpec "Foo.A" [modB] true;
in linkModuleObjects ./. modA
