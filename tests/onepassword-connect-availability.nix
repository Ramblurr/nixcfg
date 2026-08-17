{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  replicaNames = [
    "debord"
    "dewey"
    "mali"
  ];
  addresses = {
    debord = "192.0.2.21";
    dewey = "192.0.2.14";
    mali = "192.0.2.3";
  };
  virtualAddress = "192.0.2.22";
  credentialsFile = pkgs.writeText "onepassword-connect-test-secrets.yaml" "{}\n";
  user = {
    name = "op";
    uid = 3007;
  };
  group = {
    name = "op";
    gid = 3007;
  };
  evaluate =
    name:
    (inputs.nixpkgs.lib.nixosSystem {
      system = pkgs.stdenv.hostPlatform.system;
      modules = [
        inputs.quadlet-nix2.nixosModules.default
        inputs.sops-nix.nixosModules.sops
        ../modules/zfs-attrs.nix
        ../modules/services/onepassword-connect.nix
        {
          options = {
            modules.services.caddy = {
              routes = lib.mkOption {
                type = lib.types.attrs;
                default = { };
              };
              edge.certificateHosts = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ ];
              };
            };
            repo.secrets = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
            site = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
          };
        }
        {
          networking = {
            hostName = name;
            nftables.enable = true;
          };
          system.stateVersion = "26.05";
          repo.secrets = {
            global.nameservers = [ "192.0.2.53" ];
            home-ops = {
              ports.onepassword-connect-sync = 10003;
              users.onepassword-connect = user;
              groups.onepassword-connect = group;
            };
          };
          site.net.mgmt.hosts4 = (lib.mapAttrs (_: address: [ address ]) addresses) // {
            onepassword-connect = [ virtualAddress ];
          };
          systemd.network.networks."30-mgmt".addresses = [
            { Address = "${addresses.${name} or "192.0.2.99"}/24"; }
          ];
          modules.services.onepassword-connect.credentialsFile = credentialsFile;
        }
      ];
    }).config;
  configs = lib.genAttrs (replicaNames ++ [ "quine" ]) evaluate;
  enabledReplicas = lib.filter (name: configs.${name}.modules.services.onepassword-connect.enable) (
    builtins.attrNames configs
  );
  expectedPeers =
    name: lib.filter (address: address != addresses.${name}) (builtins.attrValues addresses);
  replicaIsCorrect =
    name:
    let
      cfg = configs.${name};
      service = cfg.modules.services.onepassword-connect;
      instance = cfg.services.keepalived.vrrpInstances.onepassword-connect;
      heartbeat = cfg.services.keepalived.vrrpScripts.onepassword-connect-heartbeat;
      pod = cfg.virtualisation.quadlet.pods.op-connect;
      api = cfg.virtualisation.quadlet.containers.op-connect-api;
      sync = cfg.virtualisation.quadlet.containers.op-connect-sync;
      credentials = cfg.sops.secrets."onepassword-connect/credentials";
    in
    instance == {
      interface = "mgmt";
      state = "BACKUP";
      virtualRouterId = 42;
      priority = 100;
      noPreempt = true;
      useVmac = false;
      vmacInterface = null;
      vmacXmitBase = false;
      unicastSrcIp = addresses.${name};
      unicastPeers = expectedPeers name;
      virtualIps = [
        {
          addr = virtualAddress;
          brd = null;
          dev = null;
          label = null;
          scope = null;
        }
      ];
      trackScripts = [ "onepassword-connect-heartbeat" ];
      trackInterfaces = [ ];
      extraConfig = "";
    }
    && heartbeat.interval == 5
    && heartbeat.fall == 1
    && heartbeat.rise == 1
    && lib.hasInfix "http://127.0.0.1:8080/heartbeat" heartbeat.script
    &&
      pod.podConfig.PublishPort == [
        "8080:8080"
        "127.0.0.1:10003:8081"
      ]
    &&
      api.containerConfig.Volume == [
        "/var/lib/onepassword-connect:/config:rw"
        "${credentials.path}:/config/1password-credentials.json:ro"
      ]
    && sync.containerConfig.Volume == api.containerConfig.Volume
    && credentials.sopsFile == credentialsFile
    && credentials.owner == user.name
    && credentials.group == group.name
    && credentials.mode == "0400"
    &&
      service.cacheDataset == (
        if name == "mali" then
          "rpool2/encrypted/safe/svc/onepassword-connect"
        else
          "rpool/encrypted/safe/svc/onepassword-connect"
      )
    &&
      cfg.modules.zfs.datasets.properties.${service.cacheDataset}.mountpoint
      == "/var/lib/onepassword-connect"
    && cfg.networking.firewall.interfaces.mgmt.allowedTCPPorts == [ 8080 ]
    && !cfg.services.keepalived.openFirewall
    && !(builtins.elem 10003 cfg.networking.firewall.allowedTCPPorts)
    && lib.hasInfix "iifname \"mgmt\"" cfg.networking.firewall.extraInputRules
    && lib.hasInfix "ip saddr { ${lib.concatStringsSep ", " (expectedPeers name)} }" cfg.networking.firewall.extraInputRules
    &&
      cfg.systemd.network.networks."30-mgmt".addresses == [
        { Address = "${addresses.${name}}/24"; }
      ]
    && cfg.modules.services.caddy.routes == { }
    && cfg.modules.services.caddy.edge.certificateHosts == [ ];
in
assert enabledReplicas == replicaNames;
assert lib.all replicaIsCorrect replicaNames;
pkgs.runCommand "onepassword-connect-availability-evaluation" { } "touch $out"
