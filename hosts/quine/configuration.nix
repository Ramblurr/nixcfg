{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "quine";
in {
  imports = [
    ../../modules/loginctl-linger.nix
    ./hardware-configuration.nix
    ../../profiles/interactive.nix
    ../../profiles/addon-dev.nix
    ../../profiles/addon-gaming.nix
    ../../profiles/gui-greeter.nix
    ../../profiles/gui-wayland.nix
    ../../profiles/gui-plasma.nix
    ../../profiles/gui-hyprland.nix

    ../../mixins/impermanence.nix
    ../../mixins/cpu-ryzen.nix
    ../../mixins/gfx-nvidia.nix
    ../../mixins/hyprland.nix
    ../../mixins/zfs.nix
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
  ];

  config = {
    nixpkgs.hostPlatform.system = "x86_64-linux";
    system.stateVersion = "23.05";
    #environment.systemPackages = with pkgs; [
    #];

    nixcfg.common.hostColor = "purple";
    sops.age.sshKeyPaths = ["/persist/etc/ssh/ssh_host_ed25519_key"];
    sops.defaultSopsFile = ./secrets.sops.yaml;

    #services.tailscale.useRoutingFeatures = "server";

    networking.hostName = hn;
    networking.dhcpcd.wait = "background";
    networking.dhcpcd.extraConfig = "noarp";

    systemd.network.networks."20-local-routes" = {
      matchConfig.Name = "eno1";
      linkConfig.RequiredForOnline = "routable";

      networkConfig = {
        DHCP = "yes";
        IPForward = "yes";
        # IPMasquerade = "both";
      };
      dhcpV4Config.Use6RD = "yes";
      dhcpV4Config.RouteMetric = 512;
      address = [
        "10.8.3.1/24"
        #"10.9.4.1/22"
        "10.9.8.1/23"
        "10.9.10.1/23"
        "10.8.50.1/23"
        "10.8.60.1/23"
        "10.5.0.0/24"
        "10.10.10.0/23"
      ];
      routes = [
        {
          routeConfig = {
            Gateway = "192.168.1.1";
            Metric = 2000;
          };
        }
      ];
    };

    environment.etc."machine-id".text = "76913090587c40c8a3207202dfe86fc2";

    boot = {
      #kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
      initrd = {
        availableKernelModules = ["aesni_intel" "cryptd"];

        luks.devices = {
          cryptkey = {
            device = "/dev/disk/by-label/cryptkey";
          };

          cryptswap = {
            device = "/dev/disk/by-label/cryptswap";
            keyFile = "/dev/mapper/cryptkey";
            keyFileSize = 64;
          };
        };

        postMountCommands = ''
          # Don't keep the cryptkey available all the time.
          cryptsetup close /dev/mapper/cryptkey
        '';

        postDeviceCommands = lib.mkAfter ''
          zfs rollback -r rpool/encrypted/local/root@blank && \
          zfs rollback -r rpool/encrypted/local/home@blank && \
          echo "rollback complete"
        '';
      };
    };
    #users.mutableUsers = false;
    #users.users.root.initialHashedPassword = "...";
  };
}
