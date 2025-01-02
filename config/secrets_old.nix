{
  config,
  inputs,
  lib,
  ...
}:

let
  local = config.node.secretsDir + "/local.nix";
in
{
  # Define local repo secrets
  repo.secretFiles_old = {
    global = ../secrets/global.nix;
  } // lib.optionalAttrs (lib.pathExists local) { inherit local; };
}
