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
    #../../modules/loginctl-linger.nix
    ./hardware-configuration.nix
    ../../profiles/interactive.nix
    ../../profiles/addon-dev.nix
    ../../profiles/addon-gaming.nix
    ../../profiles/gui-greeter.nix
    ../../profiles/gui-wayland.nix
    ../../profiles/gui-plasma.nix
    ../../profiles/gui-hyprland.nix

    ../../mixins/firewall.nix
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
        DNSSEC = "no";
      };

      dhcpV4Config.Use6RD = "yes";
      dhcpV4Config.RouteMetric = 512;
      domains = [
        "~socozy.casa"
        "~outskirtslabs.com"
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
    #
    deviceSpecific.vpn.mullvad.enable = true;
    deviceSpecific.vpn.tailscale.enable = true;
    deviceSpecific.backup.borgmatic = {
      enable = true;
      name = "aquinas.socozy.casa-mali";
      repositories = [
        "ssh://aquinas@borgbackup-host.int.socozy.casa/mnt/backup/borg_repos/aquinas/home"
        "\${OFFSITE_REPOSITORY}"
      ];
      exclude-patterns = [
        "etc/ssl"
        "var/home/*/.cache"
        "var/home/*/.var"
        "var/home/*/.local/lib"
        "var/home/*/.local/share/containers"
        "var/home/*/.local/share/JetBrains"
        "var/home/*/.local/share/volta"
        "var/home/*/.local/share/lein"
        "var/home/*/.local/share/Trash"
        "var/home/*/.local/share/virtualenv"
        "var/home/*/.local/share/yarn"
        "var/home/*/.local/share/nvm"
        "var/home/*/.local/state"
        "var/home/*/.npm"
        "var/home/*/.yarn"
        "var/home/*/.vagrant.d/boxes"
        "\'*.pyc\'"
        "'*/.vim*.tmp'"
        "'*/.DS_Store'"
        "'*/node_modules'"
        "'*/build'"
        "'*/target'"
        "'*/dist'"
        "'*/tmp'"
        "'*/bower_components'"
        "'*.idea'"
        "'*/.*~'"
        "'*/out'"
        "'*/.vagrant'"
        "'*/securedir'"
        "'*/encrypted'"
        "'*/ram'"
        "'*/cache'"
        "'*/.cache'"
        "'*/_cacache'"
        "'*/_lock'"
        "'*/*.tmp'"
        "'*/*.swp'"
        "'*/*~'"
        "'*/*.lock'"
        "'*/*-nas'"
        "'*/.Trash'"
        "'*/.terraform'"
        "'*/pihole-FTL.db'"
        "'*/venv'"
        "'*/emacs-doom.d'"
        "'*/SpiderOakONE'"
        "'*/.gradle'"
        "'*/.*sync*.db'"
        "'*/.ansible'"
      ];
    };
  };
}
