{ nixpkgs ? import ./nixpkgs }:
import nixpkgs {
  config = { allowUnfree = true; };
  overlays = [(import ./overlay.nix)];
}