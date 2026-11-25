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
  buildCfg = config.modules.nix.buildLimits;
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

  options.modules.nix.buildLimits = {
    enable = lib.mkEnableOption "per-build cgroups that systemd-oomd can kill one at a time";
    idleCpu = lib.mkEnableOption "builds that get CPU only when no other cgroup wants it";
    cpuWeight = lib.mkOption {
      type = lib.types.nullOr (lib.types.ints.between 1 10000);
      default = null;
      example = 20;
      description = "CPUWeight for nix-daemon.service and its builds. Lower than the default 100 without starving them like idleCpu.";
    };
    memoryHigh = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "16G";
      description = "MemoryHigh for nix-daemon.service and its builds. Above it the kernel throttles and reclaims them.";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf buildCfg.enable {
      assertions = [
        {
          assertion = !(buildCfg.idleCpu && buildCfg.cpuWeight != null);
          message = "modules.nix.buildLimits: set idleCpu or cpuWeight, not both.";
        }
      ];
      nix.settings.use-cgroups = true;
      nix.settings.experimental-features = [ "cgroups" ];
      systemd.services.nix-daemon.serviceConfig = {
        ManagedOOMMemoryPressure = "kill";
        ManagedOOMMemoryPressureLimit = "50%";
        CPUWeight =
          if buildCfg.idleCpu then
            "idle"
          else
            lib.mkIf (buildCfg.cpuWeight != null) (toString buildCfg.cpuWeight);
        MemoryHigh = lib.mkIf (buildCfg.memoryHigh != null) buildCfg.memoryHigh;
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
