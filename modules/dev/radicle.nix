{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.dev.radicle;
  inherit (config.modules.users.primaryUser) username;
  inherit (config.modules.users.primaryUser) homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
#radicle = inputs.radicle.packages.${pkgs.system}.default;
{
  options.modules.dev.radicle = {
    enable = lib.mkEnableOption "";
  };

  config = mkIf cfg.enable {
    systemd.tmpfiles.rules = mkIf withImpermanence [
      "d '/persist${homeDirectory}/.config/radicle' - ${username} ${username} - -"
    ];
    myhm = hm: {
      home.packages = [ radicle ];

      home.sessionVariables = {
        RAD_HOME = "${hm.config.xdg.configHome}/radicle";
      };

      home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
        directories = [ ".config/radicle" ];
      };

      systemd.user.services.radicle-node = {
        Unit = {
          Description = "radicle-cli node ";
          Documentation = "man:rad(1)";
        };
        Install = {
          WantedBy = [ "default.target" ];
        };
        Service = {
          Type = "simple";
          Environment = [
            "PATH=/run/wrappers/bin:/run/current-system/sw/bin:/etc/profiles/per-user/${username}/bin"
            "RAD_HOME=${hm.config.xdg.configHome}/radicle"
            "RUST_LOG=debug"
            "RUST_BACKTRACE=1"
          ];
          Restart = "always";
          RestartSec = "5";
          ExecStart = "${radicle}/bin/rad node start --foreground";
        };
      };
    };
  };
}
