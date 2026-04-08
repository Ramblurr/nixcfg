{
  config,
  lib,
  ...
}:

let
  local = config.node.secretsDir + "/local.nix";
in
{
  # Define local repo secrets
  repo.secretFiles = {
    global = lib.mkDefault ../secrets/global.nix;
  }
  // lib.optionalAttrs (lib.pathExists local) { local = lib.mkDefault local; };
}
