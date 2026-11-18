{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.impermanence;
  inherit (config.modules.users.primaryUser) username;
in
{
  options.modules.impermanence = {
    enable = lib.mkEnableOption "";
    persistHomes = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Persist the primary user's home and /root entirely.";
    };
  };

  config = lib.mkIf (cfg.enable && cfg.persistHomes) {
    environment.persistence."/persist".directories = [
      {
        directory = "/home/${username}";
        user = username;
        group = username;
        mode = "0700";
      }
      {
        directory = "/root";
        user = "root";
        group = "root";
        mode = "0700";
      }
    ];
  };
}
