{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.impermanence;
  inherit (config.modules.users.primaryUser) username;
  privateStateRoots = lib.attrNames (
    lib.filterAttrs (
      _: p: lib.any (d: lib.hasPrefix "/var/lib/private/" d.directory) p.directories
    ) config.environment.persistence
  );
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

  config = lib.mkMerge [
    (lib.mkIf (cfg.enable && cfg.persistHomes) {
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
    })
    (lib.mkIf (privateStateRoots != [ ]) {
      systemd.tmpfiles.rules = [
        "d /var/lib/private 0700 root root -"
      ]
      ++ map (root: "d ${root}/var/lib/private 0700 root root -") privateStateRoots;
    })
  ];
}
