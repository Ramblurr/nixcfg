{
  config,
  lib,
  pkgs,
  ...
}:

let
  mkHealthcheckStart = hc: "${pkgs.curl}/bin/curl --retry 3 ${hc}/start";
  mkHealthcheckReturn = hc: "${pkgs.curl}/bin/curl --retry 3 ${hc}/$?";
  mkRcloneJob =
    {
      name,
      source,
      target,
      calendar,
      extraOpts ? [ ],
      healthcheck ? null,
    }:
    {
      service = {
        "rclone-${name}" = {
          description = "Rclone backup ${name}";
          wants = [ "network-online.target" ];
          after = [ "network-online.target" ];
          script = ''
            ${lib.optionalString (healthcheck != null) mkHealthcheckStart healthcheck}
              ${pkgs.rclone}/bin/rclone \
              --config ${config.sops.secrets."rclone.conf".path} \
              --transfers 50 \
              --fast-list \
              ${lib.strings.escapeShellArgs extraOpts} \
              sync ${source} ${target}
            ${lib.optionalString (healthcheck != null) mkHealthcheckReturn healthcheck}
          '';

          serviceConfig = {
            Type = "oneshot";
            User = "root";
          };
          unitConfig = {
            RequiresMountsFor = [ "/mnt/tank2" ];
          };
        };
      };
      timer = {
        "rclone-${name}" = {
          description = "Rclone backup timer ${name}";
          timerConfig = {
            OnCalendar = calendar;
            RandomizedDelaySec = 300;
            Persistent = true;
          };
          wantedBy = [ "timers.target" ];
        };
      };
    };

  mergeServices = key: attrs: builtins.foldl' (acc: item: acc // item.${key}) { } attrs;

  # Build the rclone jobs
  # for you nix config spelunkers, this is what a rcloneJobs looks like in my secrets:
  # rcloneJobs = [
  #   {
  #     name = "google-personal";
  #     source = "google-personal:";
  #     target = "/mnt/tank2/backups/some-path";
  #     calendar = "*-*-* 14:00:00";
  #     healthcheck = "https://hc-ping.com/some-uuid"; # optional
  #     extraOpts = [ "--drive-skip-dangling-shortcuts" ]; # specific to google drive
  #   }
  # ];
  builtJobs = map mkRcloneJob config.repo.secrets.local.rcloneJobs;

  jobServices = mergeServices "service" builtJobs;
  jobTimers = mergeServices "timer" builtJobs;
in
{
  sops.secrets."rclone.conf" = { };
  systemd.services = jobServices;
  systemd.timers = jobTimers;
}
