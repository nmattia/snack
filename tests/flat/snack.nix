# This "snack" passing is ugly, figure out a nice way of passing snack-lib
with (import ../../nix {}).snack-lib;
let
  modB = mkModuleSpec "B" [] false;
  modA = mkModuleSpec "A" [modB] true;
in linkModuleObjects ./. modA
