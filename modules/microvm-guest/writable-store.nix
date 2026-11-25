{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.microvm-guest;
  inherit (cfg.writableStoreOverlay) zvolSize;
  persistent = zvolSize != null;
  rwStore = "/nix/.rw-store";
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
        # Upper-layer paths can refer to them, such as a system that
        # microvm-switch installed onto the previous image. Those paths lost
        # part of their closure for good, and nix-store --verify keeps a
        # missing path while a valid path refers to it, so they go first.
        # --load-db adds references but never drops one, so a stale row could
        # lead into the booted image; deleting there would hide an image path.
        systemd.services.nix-store-verify = {
          description = "Drop Nix database rows for store paths no layer holds";
          wantedBy = [ "multi-user.target" ];
          restartIfChanged = false;
          path = [ config.nix.package ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = ''
            set -o pipefail
            valid=$(nix --extra-experimental-features nix-command path-info --store local --all)
            gone=$(while read -r p; do [[ -e "$p" || -L "$p" ]] || echo "$p"; done <<< "$valid")
            if [[ -n "$gone" ]]; then
              doomed=$(xargs nix-store --store local --query --referrers-closure <<< "$gone" | sort -u)
              in_image=$(while read -r p; do
                ro=/nix/.ro-store/''${p#/nix/store/}
                if [[ -e "$ro" || -L "$ro" ]]; then echo "$p"; fi
              done <<< "$doomed")
              if [[ -n "$in_image" ]]; then
                echo "refusing to delete paths the booted image holds:" >&2
                echo "$in_image" >&2
                exit 1
              fi
              nix --extra-experimental-features nix-command store delete \
                --store local --ignore-liveness --stdin <<< "$doomed"
            fi
            nix-store --store local --verify
          '';
        };

        systemd.services.nix-daemon = {
          requires = [ "nix-store-verify.service" ];
          after = [ "nix-store-verify.service" ];
        };

        microvm.balloon = lib.mkDefault true;

        # The zvol is sparse, so freed blocks return to the pool only after GC and TRIM.
        nix.gc.automatic = true;
        services.fstrim.enable = true;
      })
    ]
  );
}
