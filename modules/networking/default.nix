{ options, config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let cfg = config.modules.networking.default;
in {
  options = {
    modules.networking.default = {
      enable = mkBoolOpt false;
      hostName = mkStrOpt null;
    };
  };

  config = mkIf cfg.enable {
    ## NETWORK #######################################################
    networking = {
      hostName = cfg.hostName;
      hostId = pkgs.lib.concatStringsSep "" (pkgs.lib.take 8
        (pkgs.lib.stringToCharacters (builtins.hashString "sha256" config.networking.hostName)));
      useDHCP = true;
      useNetworkd = true;
      dhcpcd.wait = "background";
      dhcpcd.extraConfig = "noarp";
    };
    services.resolved = {
      enable = true;
      dnssec = "false";
    };
    services.timesyncd.enable = true;

    systemd.network = {
      enable = true;
      wait-online.anyInterface = true;

      # leave the kernel dummy devies unmanagaed
      networks."10-dummy" = {
        matchConfig.Name = "dummy*";
        networkConfig = { };
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

      networks."99-network-defaults-wired" = {
        matchConfig.Name = "en* | eth* | usb*";
        networkConfig = {
          Description = "Fallback Wired DHCP";
          DHCP = "yes";
          IPForward = "yes";
          # IPMasquerade = "both";
        };
        # dhcpV4Config.ClientIdentifier = "mac";
        dhcpV4Config.Use6RD = "yes";
        dhcpV4Config.RouteMetric = 512;
      };
    };
  };
}
