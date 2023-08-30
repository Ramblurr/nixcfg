{
  config,
  lib,
  pkgs,
  ...
}: {
  options = {
    modules.sops.secretsFile = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Path to the decrypted SOPS secrets file.";
    };
  };
}
