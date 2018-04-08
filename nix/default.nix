{ nixpkgs ? ./nixpkgs }:
import (import nixpkgs) {
  config = { allowUnfree = true; };
  overlays = [(import ./overlay.nix)];
}
