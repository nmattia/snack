{ nixpkgs ? ./nixpkgs }:
import (import nixpkgs) {
  config = { allowUnfree = true; };
  overlays = [
    (self: super: { snack = import ../. {}; })
  ];
}
