{
  inputs,
  config,
  lib,
  pkgs,
  actual-nixpkgs,
  ...
}:

let
  inherit (config.repo.secrets.global) ciSigningPublicKey;
  heartbeatAvailable = config.site.gatus.heartbeatToken.available;
  gcCfg = config.modules.nix.gc;
  pruneProfiles = pkgs.writeShellScript "nix-gc-prune-profiles" ''
    set -euo pipefail
    cutoff=$(( $(date +%s) - ${toString gcCfg.keepDays} * 86400 ))
    mapfile -t profiles < <(
      find /nix/var/nix/profiles /root/.local/state/nix/profiles /home/*/.local/state/nix/profiles \
        -maxdepth 3 -type l -regex '.*-[0-9]+-link' 2>/dev/null | sed -E 's/-[0-9]+-link$//' | sort -u
    )
    for profile in "''${profiles[@]}"; do
      current=$(readlink "$profile" 2>/dev/null | sed -nE 's/.*-([0-9]+)-link$/\1/p')
      mapfile -t gens < <(for l in "$profile"-*-link; do [[ $l =~ ^"$profile"-([0-9]+)-link$ ]] && echo "''${BASH_REMATCH[1]}"; done | sort -n)
      count=''${#gens[@]}
      delete=()
      for i in "''${!gens[@]}"; do
        gen=''${gens[$i]}
        (( i >= count - ${toString gcCfg.keepGenerations} )) && continue
        [[ $gen == "$current" ]] && continue
        (( $(stat -c %Y "$profile-$gen-link") >= cutoff )) && continue
        delete+=("$gen")
      done
      if (( ''${#delete[@]} )); then
        echo "pruning $profile generations: ''${delete[*]}"
        ${config.nix.package}/bin/nix-env --profile "$profile" --delete-generations "''${delete[@]}"
      fi
    done
  '';
in
with lib;
{
  options.modules.nix.gc = {
    keepGenerations = lib.mkOption {
      type = lib.types.nullOr lib.types.ints.positive;
      default = 5;
      description = "Always keep this many newest generations of every profile. null keeps nix-collect-garbage's age-only pruning.";
    };
    keepDays = lib.mkOption {
      type = lib.types.ints.positive;
      default = 30;
      description = "Also keep generations younger than this many days.";
    };
  };

  options.modules.nix.buildLimits.enable =
    lib.mkEnableOption "per-build cgroups that systemd-oomd can kill one at a time";

  config = lib.mkMerge [
    (lib.mkIf config.modules.nix.buildLimits.enable {
      nix.settings.use-cgroups = true;
      nix.settings.experimental-features = [ "cgroups" ];
      systemd.services.nix-daemon.serviceConfig = {
        ManagedOOMMemoryPressure = "kill";
        ManagedOOMMemoryPressureLimit = "50%";
      };
    })
    (lib.mkIf (gcCfg.keepGenerations != null) {
      nix.gc.options = lib.mkForce "";
      systemd.services.nix-gc.preStart = "${pruneProfiles}";
    })
    {
      environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
      nix = {
        extraOptions = ''
          # Quicker timeout for inaccessible binary caches
          connect-timeout = 5
          # Do not warn on dirty git repo
          warn-dirty = false
        '';
        settings = {
          experimental-features = [
            "nix-command"
            "flakes"
          ];
          trusted-users = [
            "root"
            "@wheel"
          ];
          auto-optimise-store = true;
          extra-substituters = [
            "https://nix-community.cachix.org"
          ];
          extra-trusted-public-keys = [
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            ciSigningPublicKey
          ];
        };

        gc = {
          automatic = true;
          dates = "daily";
          options = lib.mkDefault "--delete-older-than 7d";
        };

        registry.nixpkgs.flake = actual-nixpkgs;
        registry.stable.flake = inputs.nixpkgs-stable;
        registry.unstable.flake = inputs.nixpkgs;

        nixPath = [ "nixpkgs=/etc/nixpkgs/channels/nixpkgs" ];
      };

      systemd.tmpfiles.rules = [ "L+ /etc/nixpkgs/channels/nixpkgs - - - - ${actual-nixpkgs}" ];

      programs.command-not-found.enable = false;

      system.configurationRevision = with inputs; mkIf (self ? rev) self.rev;

      # Just the bare necessities...
      environment.systemPackages =
        with pkgs;
        [
          cached-nix-shell
          dig
          jq
          curl
          vim
          wget
          unzip
          killall
          nvd
          lsof
          # These are used by the flake tooling
          git
          sops
          gnupg
          ghostty-terminfo
        ]
        ++ (map (x: x.terminfo) (
          with pkgs.pkgsBuildBuild;
          [
            kitty
            tmux
            wezterm
          ]
        ));
      system = {
        # Enable printing changes on nix build etc with nvd
        activationScripts.report-changes = ''
          PATH=$PATH:${
            lib.makeBinPath [
              pkgs.nvd
              pkgs.nix
            ]
          }
          nvd --nix-bin-dir=${config.nix.package}/bin diff $(ls -dv /nix/var/nix/profiles/system-*-link | tail -2) || true
        '';
      };

      documentation.nixos.enable = false;
      systemd.services.nix-cleanup-gcroots = {
        description = "Clean up stale Nix GC roots";
        serviceConfig.Type = "oneshot";
        script = ''
          set -eu
          # created by nix-collect-garbage, might be stale
          ${pkgs.findutils}/bin/find /nix/var/nix/temproots -type f -mtime +10 -delete
          # delete broken symlinks
          ${pkgs.findutils}/bin/find /nix/var/nix/gcroots -xtype l -delete
        '';
      };

      systemd.timers.nix-cleanup-gcroots = {
        description = "Weekly timer for nix-cleanup-gcroots";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "Sun *-*-* 03:30:00";
          Persistent = true;
        };
      };

      site.gatus.heartbeats.nix-cleanup-gcroots = lib.mkIf heartbeatAvailable {
        service = "nix-cleanup-gcroots";
        name = "Nix GC-root Cleanup";
        group = config.site.gatus.groups.infrastructure;
        interval = "192h";
      };

      site.gatus.heartbeats.nix-gc = lib.mkIf (heartbeatAvailable && config.nix.gc.automatic) {
        service = "nix-gc";
        name = "Nix Garbage Collection";
        group = config.site.gatus.groups.infrastructure;
        interval = "30h";
      };
    }
  ];
}
