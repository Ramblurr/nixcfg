# ──────────────────────────────────────────────────────────────────
# mullvad-gateway: provides mullvad access to my network
#
# This config runs a systemd nspawn container with mullvad-daemon and gost inside
# Some tricks were required to make mullvad-daemon happy inside a container.
# The benefit however is that I can namespace the VPN network and expose it via a SOCKS5 proxy to my network.
# We also expose mullvad multi-hop destinations via socks as well
# ──────────────────────────────────────────────────────────────────
{
  config,
  inputs,
  pkgs,
  lib,
  utils,
  ...
}:
let
  # these hosts can use the SOCKS proxy
  allowedCidrs = [
    "10.9.4.0/22"
    "10.4.0.2/24"
    "10.8.70.0/24"
  ];
  # these hosts have their traffic NATed through the gateway
  allowedNatCidrs = [ "10.8.70.0/24" ];
  hostAddress = "10.4.0.1";
  localAddress = "10.4.0.2";
  routesForNat = [
    {
      address = "10.8.70.0";
      prefixLength = 24;
      via = hostAddress;
    }
  ];
  defaultLocation = "de";

  mkProxy = proxy: {
    "gost-${proxy.name}" = {
      serviceConfig = {
        # these gost instances just do tcp and udp forwarding to the mullvad relay
        ExecStart = "${pkgs.gost}/bin/gost -L tcp://${localAddress}:${proxy.port}/${proxy.dest} -L udp://${localAddress}:${proxy.port}/${proxy.dest}";
      };
      wantedBy = [ "multi-user.target" ];
    };
  };

in
{
  sops.secrets = {
    "mullvad/account" = { };
  };
  environment.persistence."/persist".directories = [
    "/var/lib/mullvad-vpn"
  ];
  systemd.tmpfiles.rules = [
    "d /persist/var/lib/mullvad-vpn 0770 root root - -"
    "d /persist/var/lib/mullvad-vpn/cache 0770 root root - -"
    "d /persist/var/lib/mullvad-vpn/etc 0770 root root - -"
  ];

  # This is required to run mullvad-daemon in a container fix for mullvad-daemon to run in container, otherwise errors with:
  # Symtomps: "EPERM: Operation not permitted" "Caused by: Unable to initialize net_cls cgroup instance"
  # ref: https://github.com/mullvad/mullvadvpn-app/issues/5408#issuecomment-1805189128
  fileSystems."/tmp/net_cls" = {
    device = "net_cls";
    fsType = "cgroup";
    options = [ "net_cls" ];
  };

  containers.mullvad = {
    autoStart = true;
    ephemeral = true;
    privateNetwork = true;
    enableTun = true;
    extraFlags = [
      "--network-veth"
    ];
    hostAddress = hostAddress;
    localAddress = localAddress;
    bindMounts = {
      "/secrets/mullvad/account" = {
        hostPath = config.sops.secrets."mullvad/account".path;
        isReadOnly = true;
      };
      "/var/lib/mullvad-vpn" = {
        hostPath = "/var/lib/mullvad-vpn";
        isReadOnly = false;
      };
    };

    config =
      let
        hostPkgs = pkgs;
        hostConfig = config;
      in
      {
        config,
        pkgs,
        ...
      }:
      {
        system.stateVersion = "24.11";
        nixpkgs.pkgs = hostPkgs; # reuse host pkgs for overlays and evaluation speed
        networking.useHostResolvConf = lib.mkForce false;
        services.resolved.enable = false;
        networking.firewall.enable = true;
        networking.firewall.checkReversePath = "loose";
        networking.firewall.allowPing = true;
        networking.firewall.logRefusedConnections = true;
        networking.firewall.allowedTCPPortRanges = [
          {
            from = 1080;
            to = 1090;
          }
        ];
        networking.firewall.allowedUDPPortRanges = [
          {
            from = 1080;
            to = 1090;
          }
        ];
        networking.nat = {
          enable = true;
          internalIPs = allowedNatCidrs;
          externalInterface = "wg0-mullvad";
        };

        services.ulogd.enable = true;
        services.ulogd.settings = {
          global = {
            logfile = "/var/log/ulogd.log";
            stack = [
              "log1:NFLOG,base1:BASE,ifi1:IFINDEX,ip2str1:IP2STR,print1:PRINTPKT,emu1:LOGEMU"
            ];
          };

          log1.group = 0;

          emu1 = {
            sync = 1;
            file = "/var/log/ulogd_pkts.log";
          };
        };
        networking.interfaces.eth0.ipv4.routes =
          routesForNat
          ++ (map (
            cidr:
            let
              parts = builtins.split "/" cidr;
              addr = builtins.elemAt parts 0;
              mask = lib.toInt (builtins.elemAt parts 2);
            in
            {
              address = addr;
              prefixLength = mask;
              via = hostAddress;
            }
          ) allowedCidrs);

        environment.systemPackages = with pkgs; [
          gost
          tcpdump
        ];
        services.mullvad-vpn.enable = true;
        systemd.services = lib.mkMerge (
          [
            {
              mullvad-daemon =
                let
                  mullvad = "${config.services.mullvad-vpn.package}/bin/mullvad";
                in
                {
                  serviceConfig = {
                    LoadCredential = [ "account:/secrets/mullvad/account" ];
                    Restart = "always";
                    RestartSec = 1;
                  };
                  startLimitBurst = 5;
                  startLimitIntervalSec = 20;
                  environment = {
                    MULLVAD_SETTINGS_DIR = "/var/lib/mullvad-vpn/etc";
                    MULLVAD_CACHE_DIR = "/var/lib/mullvad-vpn/cache";
                  };
                  postStart = ''
                    while ! ${mullvad} status &>/dev/null; do sleep 1; done

                    account="$(<"$CREDENTIALS_DIRECTORY/account")"
                    current_account="$(${mullvad} account get | grep "account:" | sed 's/.* //')"

                    if [[ "$current_account" != "$account" ]]; then
                      ${mullvad} account login "$account"
                    fi

                    ${mullvad} relay set tunnel-protocol wireguard
                    ${mullvad} tunnel set wireguard --quantum-resistant off
                    ${mullvad} auto-connect set on
                    ${mullvad} lockdown-mode set off
                    ${mullvad} obfuscation set mode off
                    ${mullvad} lan set allow
                    ${mullvad} relay set location ${defaultLocation}
                    ${mullvad} connect
                  '';
                };

              # the default gost instance just opens a socks proxy so we can tunnel through the "default" mullvad location
              gost-default = {
                serviceConfig = {
                  ExecStart = "${pkgs.gost}/bin/gost -L socks5://${localAddress}:1080";
                };
                wantedBy = [ "multi-user.target" ];
              };
            }
          ]
          ++ (map mkProxy hostConfig.repo.secrets.local.mullvadProxies)

        );

      };
  };
}
