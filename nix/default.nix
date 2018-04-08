{ nixpkgs ? import ./nixpkgs }:
import nixpkgs {
  config = { allowUnfree = true; };
  overlays = [
    (self: super: { snack = import ../. {}; })
  ];
}
