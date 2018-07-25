let
  spec = builtins.fromJSON (builtins.readFile ./nixpkgs-src.json);
  rev = spec.rev;
  sha256 = spec.sha256;
  url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";

  # fetchTarball version that is compatible between all the versions of Nix
  fetch = { url, sha256 }@attrs:
    let
      inherit (builtins) lessThan nixVersion fetchTarball;
    in
    if lessThan nixVersion "1.12" then
      fetchTarball { inherit url; }
    else
      fetchTarball attrs;
in
  fetch { inherit url sha256; }
