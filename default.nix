let

  rev = "21.05"; # 2021-05-16
  #rev = "d6fe7f78a8739d825ba1176d2d3ecf596368f7a5"; # 2021-05-16
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {};

  drv = pkgs.haskellPackages.callCabal2nix "nac4" ./. {};

in if pkgs.lib.inNixShell then drv.env else drv

