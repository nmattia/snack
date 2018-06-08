{ nixpkgs ? import ./nixpkgs }:
import nixpkgs {
  config = { };
  overlays = [
    (self: super: { snack = import ../. {}; })
  ];
}
