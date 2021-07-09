{ pkgs ? (import ./nixpkgs.nix).pkgs }:
let drv = pkgs.haskellPackages.callCabal2nix "nac4" ./. {};
in if pkgs.lib.inNixShell then drv.env else drv

