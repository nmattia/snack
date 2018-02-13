# This "snack" passing is ugly, figure out a nice way of passing snack-lib
with (import ../../nix {}).snack-lib;
let
  modB = makeModuleSpec "B" [] false;
  modA = makeModuleSpec "A" [modB] true;
in linkModuleObjects ./. modA
