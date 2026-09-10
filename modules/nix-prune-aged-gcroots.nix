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
    site.gatus.heartbeats.nix-prune-aged-gcroots = lib.mkIf config.site.gatus.heartbeatToken.available {
      service = "nix-prune-aged-gcroots";
      name = "Aged Nix GC-root Pruning";
      group = config.site.gatus.groups.infrastructure;
      interval = "192h";
    };

    systemd.services.nix-prune-aged-gcroots = {
      description = "Prune aged Nix GC roots";
      serviceConfig.Type = "oneshot";
      script = ''
        set -eu
        for root in /nix/var/nix/gcroots/auto /nix/var/nix/gcroots/per-user; do
          # Nix creates these optional directories on first use.
          if [ -d "$root" ]; then
            ${pkgs.findutils}/bin/find "$root" -type l -mtime +90 -delete
          fi
        done
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
