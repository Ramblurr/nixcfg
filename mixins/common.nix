{
  config,
  lib,
  pkgs,
  inputs,
  options,
  ...
}: let
  cfg = config.nixcfg.common;
  _kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
  _zfsUnstable = false;
in {
  imports = [
    ./nix.nix
    ../profiles/user-ramblurr.nix
  ];

  options = {
    nixcfg.common = {
      defaultKernel = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          ideally, all machines run mainline. this is mostly disabled for mobile-nixos devices
          (also, in most cases linuxPackages could just be overridden directly)
          # TODO: it would be nice if mobile-nixos didn't make me need this...
        '';
      };
      defaultNoDocs = lib.mkOption {
        type = lib.types.bool;
        default = true;
      };
      defaultNetworking = lib.mkOption {
        type = lib.types.bool;
        default = true;
      };
      useZfs = lib.mkOption {
        type = lib.types.bool;
        default = true;
      };
      usePlymouth = lib.mkOption {
        type = lib.types.bool;
        default = true;
      };
      hostColor = lib.mkOption {
        type = lib.types.str;
        default = "grey";
        description = "this is used as a hostname-hint-accent in zellij/waybar/shell prompts";
      };
      skipMitigations = lib.mkOption {
        type = lib.types.bool;
        default = true;
      };
    };
  };

  config = {
    ## DEBLOAT ##############################################################
    documentation = lib.mkIf cfg.defaultNoDocs {
      enable = false;
      doc.enable = false;
      man.enable = false;
      info.enable = false;
      nixos.enable = false;
    };
    system.disableInstallerTools = true;

    ## BOOT #################################################################
    console.earlySetup = true; # needed for LUKS
    boot = {
      tmp.useTmpfs = lib.mkDefault false;
      tmp.cleanOnBoot = true;
      zfs.enableUnstable = cfg.useZfs && _zfsUnstable;

      loader = {
        efi = {
          canTouchEfiVariables = true;
        };
        systemd-boot = {
          enable = lib.mkDefault true;
          configurationLimit = 20;
        };
        timeout = 3;
      };

      plymouth.enable = cfg.usePlymouth;
      initrd.systemd.enable = true;
      initrd.supportedFilesystems = lib.optionals (cfg.useZfs) ["zfs"];

      kernelPackages = lib.mkIf cfg.defaultKernel _kernelPackages;
      kernelParams = lib.mkIf cfg.skipMitigations ["mitigations=off"];
      kernel.sysctl = {
        "fs.file-max" = 100000;
        "fs.inotify.max_user_instances" = 256;
        "fs.inotify.max_user_watches" = 99999999;
      };
    };

    ## LEGACYBOOT - we use stage-1/systemd so have a fallback ###############
    specialisation."legacyboot" = lib.mkIf (config.boot.initrd.systemd.enable) {
      inheritParentConfig = true;
      configuration = {
        boot.initrd.systemd.enable = lib.mkForce false;
        boot.initrd.luks.devices."nixos-luksroot".fallbackToPassword = true;
      };
    };

    ## NETWORK + TIME #######################################################
    networking = {
      hostId = pkgs.lib.concatStringsSep "" (pkgs.lib.take 8
        (pkgs.lib.stringToCharacters
          (builtins.hashString "sha256" config.networking.hostName)));
      firewall.enable = true;
      useDHCP = lib.mkIf (cfg.defaultNetworking) false;
      useNetworkd = lib.mkIf (cfg.defaultNetworking) true;

      firewall.logRefusedConnections = false;
    };
    services.resolved = {
      enable = true;
    };
    services.timesyncd.enable = true;
    time.timeZone = lib.mkDefault "Europe/Berlin";

    systemd.network = lib.mkIf (cfg.defaultNetworking) {
      enable = true;

      wait-online.anyInterface = true;

      # leave the kernel dummy devies unmanagaed
      networks."10-dummy" = {
        matchConfig.Name = "dummy*";
        networkConfig = {};
        # linkConfig.ActivationPolicy = "always-down";
        linkConfig.Unmanaged = "yes";
      };

      networks."20-tailscale-ignore" = {
        matchConfig.Name = "tailscale*";
        linkConfig = {
          Unmanaged = "yes";
          RequiredForOnline = false;
        };
      };

      networks."30-network-defaults-wired" = {
        matchConfig.Name = "en* | eth* | usb*";
        networkConfig = {
          DHCP = "yes";
          IPv6AcceptRA = true;
          DHCPv6PrefixDelegation = "yes";
          IPForward = "yes";
          # IPMasquerade = "both";
        };
        # dhcpV4Config.ClientIdentifier = "mac";
        dhcpV4Config.Use6RD = "yes";
        dhcpV4Config.RouteMetric = 512;
        dhcpV6Config.RouteMetric = 512;
        dhcpV6Config.PrefixDelegationHint = "::64";
      };
      networks."30-network-defaults-wireless" = {
        matchConfig.Name = "wl*";
        networkConfig = {
          DHCP = "yes";
          IPv6AcceptRA = true;
          DHCPv6PrefixDelegation = "yes";
          IPForward = "yes";
          # IPMasquerade = "both";
        };
        # dhcpV4Config.ClientIdentifier = "mac";
        dhcpV4Config.RouteMetric = 1500;
        dhcpV6Config.RouteMetric = 1500;
        # routes = [
        #   { routeConfig = { Gateway = "_dhcp4"; Metric = 1500; }; }
        #   { routeConfig = { Gateway = "_ipv6ra"; Metric = 1500; }; }
        # ];
        dhcpV4Config.Use6RD = "yes";
        dhcpV6Config.PrefixDelegationHint = "::64";
      };
    };

    security = {
      sudo.enable = true;
      sudo.wheelNeedsPassword = false;

      please.enable = true;
      please.wheelNeedsPassword = false;
    };

    users = {
      mutableUsers = false;
      users."root".initialHashedPassword = lib.mkForce "$6$EiATeVQpuAcM4BfT$B31xDzJjYs4pXF500/rOB.pVo1N8PJ9NEt0OF/U04Jw4n.DFfw2sjt71rbN3xqtVVSUBe0rN4qKVGILWQE6Xu0";
      users."root".hashedPassword = config.users.users."root".initialHashedPassword;
    };

    ## MISC HARDWARE RELATED ################################################
    services.fwupd.enable = true;
    services.udisks2.enable = true;
    hardware.enableRedistributableFirmware = true;
    hardware.usbWwan.enable = false; # dual role usb/cdrom stick thing
    hardware.cpu.amd.updateMicrocode = pkgs.hostPlatform.system == "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = pkgs.hostPlatform.system == "x86_64-linux";

    environment = {
      systemPackages = with pkgs; [coreutils];
      etc."flake.lock" = {
        source = ../flake.lock;
      };
    };

    ## SILLY CUSTOMIZATION ##################################################
    services.getty = {
      greetingLine = ''\l  -  (kernel: \r) (label: ${config.system.nixos.label}) (arch: \m)'';
      helpLine = ''
        -... . / --. .- -.-- --..-- / -.. --- / -.-. .-. .. -- .
      '';
    };
  };
}
