{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  mkEvaluated =
    reconcileConfig:
    inputs.nixpkgs.lib.nixosSystem {
      system = pkgs.stdenv.hostPlatform.system;
      modules = [
        ../modules/zfs-attrs.nix
        inputs.sops-nix.nixosModules.sops
        ../modules/services/onepassword-systemd-credentials.nix
        ../modules/site/gatus.nix
        ../modules/site/gatus-heartbeats.nix
        ../modules/site/gatus-heartbeats-onepassword.nix
        ../hosts/mali/zrepl-receiver-reconcile.nix
        {
          options.repo.secrets = lib.mkOption { type = lib.types.attrs; };
        }
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName = "mali";
          repo.secrets.global.domain.home = "example.test";
          system.stateVersion = "26.05";
          boot.loader.grub.devices = [ "nodev" ];
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
          modules.zfs.datasets.enable = true;
          modules.services.onepassword-systemd-credentials = {
            enable = true;
            bootstrapTokenFile = "/run/onepassword-connect-token";
            connectHost = "http://192.0.2.22:8080";
            package = pkgs.writeShellScriptBin "op" "exit 1";
          };
          services.rsyncnet-zrepl-reconcile = reconcileConfig;
        }
      ];
    };
  validConfig = {
    enable = true;
    timer.enable = true;
    receiverHost = "receiver.example.invalid";
    identityReference = "op://test/mali-rsyncnet-reconciler/private key";
    knownHostsReference = "op://test/mali-rsyncnet-reconciler/known hosts";
  };
  evaluated = mkEvaluated validConfig;
  invalidHostEvaluation =
    builtins.tryEval
      (mkEvaluated (validConfig // { receiverHost = "bad host"; })).config.system.build.toplevel.drvPath;
  invalidReferenceEvaluation =
    builtins.tryEval
      (mkEvaluated (validConfig // { identityReference = "not-an-op-reference"; }))
      .config.system.build.toplevel.drvPath;
  provider = evaluated.config.modules.services.onepassword-systemd-credentials;
  service = evaluated.config.systemd.services.rsyncnet-zrepl-reconcile;
  timer = evaluated.config.systemd.timers.rsyncnet-zrepl-reconcile;
  unscheduled = mkEvaluated (validConfig // { timer.enable = false; });
  unscheduledTimer = unscheduled.config.systemd.timers.rsyncnet-zrepl-reconcile;
  inherit (service) serviceConfig;
  stateDataset =
    evaluated.config.modules.zfs.datasets.properties."rpool2/encrypted/safe/svc/zrepl-reconcile";
in
assert !invalidHostEvaluation.success;
assert !invalidReferenceEvaluation.success;
assert serviceConfig.Type == "oneshot";
assert serviceConfig.DynamicUser;
assert serviceConfig.User == "rsyncnet-zrepl";
assert !(builtins.hasAttr "rsyncnet-zrepl-reconcile" evaluated.config.users.users);
assert !(builtins.hasAttr "rsyncnet-zrepl" evaluated.config.users.users);
assert serviceConfig.NoNewPrivileges;
assert serviceConfig.ProtectSystem == "strict";
assert serviceConfig.StateDirectory == "rsyncnet-zrepl-reconcile";
assert serviceConfig.StateDirectoryMode == "0700";
assert serviceConfig.RuntimeDirectory == "rsyncnet-zrepl-reconcile";
assert builtins.elem "AF_UNIX" serviceConfig.RestrictAddressFamilies;
assert service.unitConfig.RequiresMountsFor == [ "/var/lib/private/rsyncnet-zrepl-reconcile" ];
assert
  stateDataset == {
    atime = "off";
    compression = "zstd";
    mountpoint = "/var/lib/private/rsyncnet-zrepl-reconcile";
  };
assert service.environment.EXPECTED_BUNDLE_ID == "v1-c337a0f46626b904-initial";
assert service.environment.SSH_DEADLINE_SECONDS == "900";
assert !(service.environment ? STATE_DIRECTORY);
assert !(service.environment ? RUNTIME_DIRECTORY);
assert
  provider.consumers.rsyncnet-zrepl-reconcile == {
    identity = "op://test/mali-rsyncnet-reconciler/private key";
    gatus-token = "op://home-ops-prod/gatus/borgmatic_external_endpoint_token";
    known-hosts = "op://test/mali-rsyncnet-reconciler/known hosts";
  };
assert
  serviceConfig.LoadCredential == [
    "gatus-token:${provider.socketPath}"
    "identity:${provider.socketPath}"
    "known-hosts:${provider.socketPath}"
  ];
assert
  evaluated.config.site.gatus.externalEndpoints == [
    {
      name = "rsync.net Zrepl Receiver Reconciliation (mali)";
      group = evaluated.config.site.gatus.groups.infrastructure;
      token = "$GATUS_EXTERNAL_TOKEN";
      heartbeat.interval = "45m";
      alerts = [ { type = "pushover"; } ];
    }
  ];
assert lib.hasPrefix "+" serviceConfig.ExecStopPost;
assert lib.hasInfix "--name 'rsync.net Zrepl Receiver Reconciliation (mali)'"
  serviceConfig.ExecStopPost;
assert builtins.elem "onepassword-credential-provider.socket" service.requires;
assert builtins.elem "onepassword-credential-provider.socket" service.after;
assert builtins.elem "${pkgs.coreutils}/bin/test -s %d/identity" serviceConfig.ExecStartPre;
assert builtins.elem "${pkgs.coreutils}/bin/test -s %d/known-hosts" serviceConfig.ExecStartPre;
assert service.wantedBy == [ ];
assert timer.wantedBy == [ "timers.target" ];
assert unscheduledTimer.wantedBy == [ ];
assert unscheduled.config.site.gatus.externalEndpoints == [ ];
assert timer.timerConfig.OnCalendar == "*:0/15";
assert timer.timerConfig.RandomizedDelaySec == "2min";
assert timer.timerConfig.Persistent;
assert !lib.hasInfix "rsyncnet.key" (builtins.toString serviceConfig.ExecStart);
assert !(builtins.elem "rsyncnet.key" (builtins.attrNames service.environment));
pkgs.runCommand "mali-zrepl-reconcile-test"
  {
    nativeBuildInputs = [
      pkgs.bash
      pkgs.shellcheck
      pkgs.shfmt
    ];
  }
  ''
    bash -n ${../scripts/rsyncnet-zrepl-reconcile/reconcile.sh} ${../scripts/rsyncnet-zrepl-reconcile/tests/run.sh}
    shellcheck -x ${../scripts/rsyncnet-zrepl-reconcile/reconcile.sh} ${../scripts/rsyncnet-zrepl-reconcile/tests/run.sh}
    shfmt -d -i 2 -ci ${../scripts/rsyncnet-zrepl-reconcile/reconcile.sh} ${../scripts/rsyncnet-zrepl-reconcile/tests/run.sh}
    test -x ${serviceConfig.ExecStart}
    ${pkgs.bash}/bin/bash ${../.}/scripts/rsyncnet-zrepl-reconcile/tests/run.sh
    touch "$out"
  ''
