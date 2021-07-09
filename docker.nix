{ pkgs ? (import ./nixpkgs.nix).pkgs }:
let
  drv = pkgs.haskellPackages.callCabal2nix "nac4" ./. {};
  app = pkgs.haskell.lib.justStaticExecutables drv;
  entrypoint = pkgs.writeScript "entrypoint.sh" ''
    #!${pkgs.stdenv.shell}
    $@
  '';
in
  pkgs.dockerTools.buildLayeredImage {
    name = "not-a-connect4";
    tag = "latest";
    config = {
      WorkingDir = "${app}";
      Entrypoint = [ entrypoint ];
      Cmd = [ "${app}/bin/nac4-server" ];
    };
  }

