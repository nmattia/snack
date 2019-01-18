with { fetch = import ./fetch.nix; };
{ nixpkgs ? fetch.nixpkgs }:
import nixpkgs {
  config = { };
  overlays = [
    (import ./overlay.nix)
  ];
}
