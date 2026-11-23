{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.microvm-guest;
  inherit (cfg.writableStoreOverlay) zvolSize;
  persistent = zvolSize != null;
  rwStore = "/nix/.rw-store";
  inherit (config.system.build) toplevel;

  # Runs on the VM host, which holds the closure after deploy copies this
  # script there. The guest cannot reach the host store, so the missing paths
  # are streamed over the same SSH channel.
  switcher = pkgs.writeShellApplication {
    name = "microvm-switch";
    text = ''
      target=''${1:?usage: microvm-switch root@<guest>}
      remote_name=$(ssh -n "$target" cat /etc/hostname)
      [[ "$remote_name" == ${lib.escapeShellArg config.networking.hostName} ]] \
        || { echo "refusing to switch $remote_name to ${config.networking.hostName}" >&2; exit 1; }

      # The guest GC holds gc.lock exclusively while it runs. A shared lock
      # held until this script exits keeps it from deleting paths between the
      # validity check and the profile update. Closing stdin releases it.
      coproc gc_lock { ssh "$target" "flock -s /nix/var/nix/gc.lock sh -c 'echo locked; exec cat'"; }
      read -r _ <&"''${gc_lock[0]}"

      missing=$(nix-store --query --requisites ${toplevel} \
        | ssh "$target" xargs nix-store --check-validity --print-invalid)
      if [[ -n "$missing" ]]; then
        readarray -t paths <<< "$missing"
        echo "Copying ''${#paths[@]} store paths to $target"
        nix-store --export "''${paths[@]}" | ssh "$target" nix-store --import >/dev/null
      fi
      ssh -n "$target" nix-env -p /nix/var/nix/profiles/system --set ${toplevel}
      ssh -n "$target" ${toplevel}/bin/switch-to-configuration switch
    '';
  };
in
{
  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        microvm.writableStoreOverlay = lib.mkIf (cfg.writableStoreOverlay.enable || persistent) rwStore;
      }
      (lib.mkIf persistent {
        assertions = [
          {
            assertion = config.microvm.storeOnDisk;
            message = "writableStoreOverlay.zvolSize requires nixStoreBackend = \"blk\"; the host /nix/store must not be shared.";
          }
          {
            assertion = config.microvm.vsock.ssh.enable;
            message = "writableStoreOverlay.zvolSize requires VSOCK SSH so deploy can hot-switch the guest.";
          }
        ];

        modules.microvm-guest.zvols.nix-rw = {
          size = zvolSize;
          mountPoint = rwStore;
        };

        fileSystems."/nix/var" = {
          device = "${rwStore}/var";
          fsType = "none";
          options = [ "bind" ];
          depends = [ rwStore ];
          neededForBoot = true;
        };

        # An upper-layer copy of a path the image now ships shadows the image
        # copy forever, so drop it before the overlay is assembled. This also
        # clears whiteouts that GC left for image paths. The image builder
        # stores a symlink path as a copy of its target, so only an entry of
        # the same type counts as a duplicate; the upper copy keeps the
        # content the database hash describes.
        boot.initrd.systemd.services.nix-rw-store-dedup = {
          description = "Create /nix/var and drop upper-layer store paths the image ships";
          requiredBy = [
            "sysroot-nix-store.mount"
            "sysroot-nix-var.mount"
          ];
          before = [
            "sysroot-nix-store.mount"
            "sysroot-nix-var.mount"
          ];
          unitConfig = {
            DefaultDependencies = false;
            RequiresMountsFor = [
              "/sysroot/nix/.ro-store"
              "/sysroot${rwStore}"
            ];
          };
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = ''
            mkdir -p /sysroot${rwStore}/var
            for p in /sysroot${rwStore}/store/*; do
              ro=/sysroot/nix/.ro-store/''${p##*/}
              if [ -c "$p" ]; then
                [ -e "$ro" ] || [ -L "$ro" ] || continue
              elif [ -L "$p" ]; then
                [ -L "$ro" ] || continue
              elif [ -d "$p" ]; then
                [ -d "$ro" ] && ! [ -L "$ro" ] || continue
              elif [ -f "$p" ]; then
                [ -f "$ro" ] && ! [ -L "$ro" ] || continue
              else
                continue
              fi
              rm -rf "$p"
            done
          '';
        };

        # Stage 2 runs nix-store --load-db for the booted image. Rows for
        # paths that only the previous image held stay valid until verified.
        systemd.services.nix-store-verify = {
          description = "Drop Nix database rows for store paths no layer holds";
          wantedBy = [ "multi-user.target" ];
          restartIfChanged = false;
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStart = "${config.nix.package}/bin/nix-store --verify";
          };
        };

        systemd.services.nix-daemon = {
          requires = [ "nix-store-verify.service" ];
          after = [ "nix-store-verify.service" ];
        };

        system.switch.enable = true;
        microvm.deploy.sshSwitch = switcher;
        microvm.balloon = lib.mkDefault true;

        # The zvol is sparse, so freed blocks return to the pool only after GC and TRIM.
        nix.gc.automatic = true;
        services.fstrim.enable = true;
      })
    ]
  );
}
