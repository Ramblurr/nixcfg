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
  repo.secretFiles = {
    global = ../secrets/global.nix;
  } // lib.optionalAttrs (lib.pathExists local) { inherit local; };
}
