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
  cfg = config.modules.server.smtp-external-relay;
  hostName = config.networking.hostName;
in
{
  options.modules.server.smtp-external-relay = {
    enable = lib.mkEnableOption "";
    emailTo = lib.mkOption {
      type = lib.types.uniq lib.types.str;
      default = config.repo.secrets.global.email.work;
    };
    emailFrom = lib.mkOption {
      type = lib.types.uniq lib.types.str;
      default = config.repo.secrets.global.email.home;
    };
    smtpHost = lib.mkOption {
      type = lib.types.uniq lib.types.str;
      default = config.repo.secrets.global.email.siteRelay;
    };
  };
  config =
    let
      sendEmailEvent =
        { event }:
        ''
          printf "Subject: ${hostName} ${event} $(${pkgs.coreutils}/bin/date --iso-8601=seconds)\n\nzpool status:\n\n$(${pkgs.zfs}/bin/zpool status)" | ${pkgs.msmtp}/bin/msmtp -a default ${cfg.emailTo}
        '';
    in
    mkIf cfg.enable {
      programs.msmtp = {
        enable = true;
        setSendmail = true;
        defaults = {
          aliases = builtins.toFile "aliases" ''
            default: ${cfg.emailTo}
          '';
        };
        accounts = {
          default = {
            host = cfg.smtpHost;
            port = 25;
            from = cfg.emailFrom;
            auth = "off";
          };
        };
      };
      systemd.services."boot-mail-alert" = {
        wantedBy = [ "multi-user.target" ];
        wants = [ "network-online.target" ];
        after = [
          "network.target"
          "network-online.target"
        ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script = sendEmailEvent { event = "just booted"; };
      };
      systemd.services."shutdown-mail-alert" = {
        wantedBy = [ "multi-user.target" ];
        wants = [ "network-online.target" ];
        after = [
          "network.target"
          "network-online.target"
        ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script = "true";
        preStop = sendEmailEvent { event = "is shutting down"; };
      };
      systemd.services."weekly-mail-alert" = {
        serviceConfig.Type = "oneshot";
        script = sendEmailEvent { event = "is still alive"; };
      };
      systemd.timers."weekly-mail-alert" = {
        wantedBy = [ "timers.target" ];
        partOf = [ "weekly-mail-alert.service" ];
        timerConfig.OnCalendar = "weekly";
      };

      services.smartd.notifications.mail.enable = true;
      services.smartd.notifications.mail.sender = cfg.emailFrom;
      services.smartd.notifications.mail.recipient = cfg.emailTo;
    };
}
