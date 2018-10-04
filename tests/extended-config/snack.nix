rec {
  pkgs = import ./nix;
  ghcWithPackages = pkgs.haskellPackages.ghcWithPackages;
}
