{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.nix.pruneAgedGcroots;
in
{
  options.modules.nix.pruneAgedGcroots.enable =
    lib.mkEnableOption "age-based cleanup of automatic and per-user Nix GC roots";

  config = lib.mkIf cfg.enable {
    systemd.services.nix-prune-aged-gcroots = {
      description = "Prune aged Nix GC roots";
      serviceConfig.Type = "oneshot";
      script = ''
        set -eu
        ${pkgs.findutils}/bin/find /nix/var/nix/gcroots/auto /nix/var/nix/gcroots/per-user -type l -mtime +90 -delete || true
      '';
    };

    systemd.timers.nix-prune-aged-gcroots = {
      description = "Weekly timer for nix-prune-aged-gcroots";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "Sun *-*-* 03:30:00";
        Persistent = true;
      };
    };
  };
}
