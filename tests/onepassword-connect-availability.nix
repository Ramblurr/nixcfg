{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  replicaNames = [
    "debord"
    "dewey"
    "mali"
  ];
  inherit (import ./onepassword-connect-eval.nix { inherit inputs pkgs; })
    addresses
    virtualAddress
    credentialsFile
    user
    group
    evaluate
    ;
  tls = import ../configs/home-ops/onepassword-connect-tls.nix;
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
      endpoints = cfg.services.tailscale.serve.services.op-connect.endpoints;
      pod = cfg.virtualisation.quadlet.pods.op-connect;
      api = cfg.virtualisation.quadlet.containers.op-connect-api;
      sync = cfg.virtualisation.quadlet.containers.op-connect-sync;
      credentials = cfg.sops.secrets."onepassword-connect/credentials";
      tlsKey = cfg.sops.secrets."onepassword-connect/tls-key";
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
    && heartbeat.user == user.name
    && heartbeat.group == group.name
    && lib.hasInfix "-onepassword-connect.crt https://127.0.0.1:9443/heartbeat" heartbeat.script
    && lib.hasInfix "--cacert /nix/store/" heartbeat.script
    && endpoints == { "tcp:443" = "https://${virtualAddress}:9443"; }
    && lib.hasInfix "onepassword-connect-trust" cfg.systemd.services.tailscaled.environment.SSL_CERT_DIR
    &&
      pod.podConfig.PublishPort == [
        "9443:9443"
        "127.0.0.1:10003:8081"
      ]
    && lib.all (variable: builtins.elem variable api.containerConfig.Environment) [
      "OP_HTTPS_PORT=9443"
      "OP_TLS_CERT_FILE=/tls/certificate.pem"
      "OP_TLS_KEY_FILE=/tls/key.pem"
    ]
    &&
      api.containerConfig.Volume == [
        "/var/lib/onepassword-connect:/config:rw"
        "${credentials.path}:/config/1password-credentials.json:ro"
        "${tls.certificate}:/tls/certificate.pem:ro"
        "${tlsKey.path}:/tls/key.pem:ro"
      ]
    &&
      sync.containerConfig.Volume == [
        "/var/lib/onepassword-connect:/config:rw"
        "${credentials.path}:/config/1password-credentials.json:ro"
      ]
    && tlsKey.sopsFile == tls.keyFile
    && tlsKey.owner == user.name
    && tlsKey.mode == "0400"
    && !(builtins.elem "sops-install-secrets.service" (api.unitConfig.Requires or [ ]))
    && !(builtins.elem "sops-install-secrets.service" (api.unitConfig.After or [ ]))
    && !(builtins.elem "sops-install-secrets.service" (sync.unitConfig.Requires or [ ]))
    && !(builtins.elem "sops-install-secrets.service" (sync.unitConfig.After or [ ]))
    && credentials.sopsFile == credentialsFile
    && credentials.owner == user.name
    && credentials.group == group.name
    && credentials.mode == "0400"
    && credentials.restartUnits == [ ]
    && cfg.users.users.${user.name}.shell == pkgs.bashInteractive
    &&
      cfg.home-manager.users.${user.name}.home.sessionVariables.DBUS_SESSION_BUS_ADDRESS
      == "unix:path=/run/user/${toString user.uid}/bus"
    &&
      cfg.home-manager.users.${user.name}.home.sessionVariables.XDG_RUNTIME_DIR
      == "/run/user/${toString user.uid}"
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
    && cfg.networking.firewall.interfaces.mgmt.allowedTCPPorts == [ 9443 ]
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
