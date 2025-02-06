# This config runs a systemd nspawn container with mullvad-daemon and gost inside
# Some tricks were required to make mullvad-daemon happy inside a container.
# The benefit however is that I can namespace the VPN network and expose it via a SOCKS5 proxy to my network.
# We also expose mullvad multi-hop destinations via socks as well
{
  config,
  inputs,
  pkgs,
  lib,
  utils,
  ...
}:
let
  allowedCidrs = [
    "10.9.4.0/22"
    "10.4.0.2/24"
    "10.8.70.0/24"
  ];
  hostAddress = "10.4.0.1";
  localAddress = "10.4.0.2";
  defaultLocation = "de";

  proxies = [
    {
      name = "pl1";
      port = "1081";
      dest = "pl-waw-wg-socks5-103.relays.mullvad.net:1080";
    }
    {
      name = "bg1";
      port = "1082";
      dest = "bg-sof-wg-socks5-001.relays.mullvad.net:1080";
    }
  ];

  mkProxy = proxy: {
    "gost-${proxy.name}" = {
      serviceConfig = {
        # these gost instances just do tcp forwarding to the mullvad relay
        ExecStart = "${pkgs.gost}/bin/gost -L tcp://${localAddress}:${proxy.port}/${proxy.dest} -L udp://${localAddress}:${proxy.port}/${proxy.dest}";
      };
      wantedBy = [ "multi-user.target" ];
    };
  };

in
{
  sops.secrets = {
    "mullvad/account" = { };
    "mullvad/device" = { };
    "mullvad/privateKey" = { };
    "mullvad/server" = { };
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
    # enableTun = true; # NOTE doesn't work https://github.com/NixOS/nixpkgs/pull/357276
    allowedDevices = [
      {
        # what enableTun should do
        modifier = "rwm";
        node = "/dev/net/tun";
      }
    ];
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
      in
      {
        config,
        pkgs,
        ...
      }:
      {
        #imports = [ inputs.nixos-nftables-firewall.nixosModules.default ];
        system.stateVersion = "24.11";
        nixpkgs.pkgs = hostPkgs; # reuse host pkgs for overlays and evaluation speed
        networking.useHostResolvConf = lib.mkForce false;
        services.resolved.enable = true;
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
          internalIPs = [ "10.8.70.0/24" ];
          externalInterface = "wg0-mullvad";
        };

        services.ulogd.enable = true;
        services.ulogd.settings = {
          global = {
            logfile = "/var/log/ulogd.log";
            stack = [
              "log1:NFLOG,base1:BASE,ifi1:IFINDEX,ip2str1:IP2STR,print1:PRINTPKT,emu1:LOGEMU"
              "log1:NFLOG,base1:BASE,pcap1:PCAP"
            ];
          };

          log1.group = 0;

          pcap1 = {
            sync = 1;
            file = "/var/log/ulogd.pcap";
          };

          emu1 = {
            sync = 1;
            file = "/var/log/ulogd_pkts.log";
          };
        };
        #networking.firewall.enable = false;
        #networking.nat.enable = false;
        #networking.nftables.enable = true;
        #networking.nftables = {
        #  chains =
        #    let
        #      dropRule = label: {
        #        after = lib.mkForce [ "veryLate" ];
        #        before = lib.mkForce [ "end" ];
        #        rules = lib.singleton ''counter log prefix "mullvad_drop_${label} " group 0 accept comment "Default drop rule for ${label} chain"'';
        #      };
        #    in
        #    {
        #      input.drop = dropRule "input";
        #      forward.drop = dropRule "forward";
        #    };
        #  firewall = {
        #    enable = true;
        #    snippets = {
        #      nnf-common.enable = false;
        #      nnf-conntrack.enable = true;
        #      nnf-default-stopRuleset.enable = true;
        #      nnf-drop.enable = false;
        #      nnf-loopback.enable = true;
        #      nnf-dhcpv6.enable = false;
        #      nnf-icmp.enable = true;
        #      nnf-ssh.enable = true;
        #      nnf-nixos-firewall.enable = true;
        #    };
        #    zones = {
        #      eth0.interfaces = [ "eth0" ];
        #      wg0.interfaces = [ "wg0-mullvad" ];
        #      clients.ipv4Addresses = allowedCidrs;
        #      routingClients.ipv4Addresses = [ "10.8.70.0/24" ];
        #    };
        #    rules = {
        #      allow_internal = {
        #        from = [ "clients" ];
        #        to = [ "fw" ];
        #        extraLines = [
        #          ''counter log prefix "allow_internal " group 0 accept''
        #        ];
        #      };
        #      wan_ingress = {
        #        from = [ "wg0" ];
        #        to = "all";
        #        ruleType = "policy";
        #        extraLines = [
        #          ''counter log prefix "wan_ingress " group 0 ''
        #        ];
        #      };
        #      wan_egress = {
        #        from = [ "routingClients" ];
        #        to = [ "wg0" ];
        #        verdict = "accept";
        #        late = true;
        #        masquerade = true;
        #      };
        #    };
        #  };
        #};

        networking.interfaces.eth0.ipv4.routes =
          [
            {
              address = "10.8.70.0";
              prefixLength = 24;
              via = hostAddress;
            }
          ]
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
                    ${mullvad} relay set location ${defaultLocation}
                    ${mullvad} auto-connect set on
                    ${mullvad} lockdown-mode set on
                    ${mullvad} lan set allow
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
          ++ (map mkProxy proxies)

        );

      };
  };
}
