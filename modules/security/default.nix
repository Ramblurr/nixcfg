{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.security.default;
in
{
  options = {
    modules.security.default = {
      enable = lib.mkOption {
        type = types.bool;
        default = true;
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.extraConfig = "DefaultLimitNOFILE=1048576";
    security = {
      sudo.enable = true;
      sudo.wheelNeedsPassword = false;
      sudo.extraRules =
        let
          # systemPath is the path where the system being activated is uploaded by `deploy`.
          systemPath = "/nix/store/*-activatable-nixos-system-${config.networking.hostName}-*";
          nopasswd = command: {
            inherit command;
            options = [
              "NOPASSWD"
              "SETENV"
            ];
          };
        in
        [
          {
            groups = [ "wheel" ];
            runAs = "root";
            commands = [ (nopasswd "/run/current-system/sw/bin/systemctl reboot") ];
          }
        ];
      #pam.loginLimits = [
      #  {
      #    domain = "*";
      #    type = "soft";
      #    item = "nofile";
      #    value = "1048576";
      #  }
      #];
    };
  };
}
