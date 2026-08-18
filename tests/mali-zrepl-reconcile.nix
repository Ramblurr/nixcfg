{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  mkEvaluated =
    reconcileConfig:
    inputs.nixpkgs.lib.nixosSystem {
      system = pkgs.stdenv.hostPlatform.system;
      modules = [
        inputs.impermanence.nixosModules.impermanence
        inputs.sops-nix.nixosModules.sops
        ../modules/services/onepassword-systemd-credentials.nix
        ../hosts/mali/zrepl-receiver-reconcile.nix
        {
          nixpkgs.pkgs = pkgs;
          system.stateVersion = "26.05";
          boot.loader.grub.devices = [ "nodev" ];
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
          environment.persistence."/persist".directories = [ "/var/lib/nixos" ];
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
  unscheduledTimer =
    (mkEvaluated (validConfig // { timer.enable = false; }))
    .config.systemd.timers.rsyncnet-zrepl-reconcile;
  inherit (service) serviceConfig;
  persistedState = lib.findFirst (
    entry: !builtins.isString entry && entry.directory == "/var/lib/rsyncnet-zrepl-reconcile"
  ) null evaluated.config.environment.persistence."/persist".directories;
in
assert !invalidHostEvaluation.success;
assert !invalidReferenceEvaluation.success;
assert serviceConfig.Type == "oneshot";
assert serviceConfig.User == "rsyncnet-zrepl-reconcile";
assert serviceConfig.NoNewPrivileges;
assert serviceConfig.ProtectSystem == "strict";
assert serviceConfig.StateDirectoryMode == "0700";
assert persistedState != null;
assert persistedState.user == "rsyncnet-zrepl-reconcile";
assert persistedState.group == "rsyncnet-zrepl-reconcile";
assert persistedState.mode == "0700";
assert service.environment.EXPECTED_BUNDLE_ID == "v1-ccc29d6eb3b5a463-initial";
assert service.environment.SSH_DEADLINE_SECONDS == "900";
assert service.environment.STATE_DIRECTORY == "/var/lib/rsyncnet-zrepl-reconcile";
assert
  provider.consumers.rsyncnet-zrepl-reconcile == {
    identity = "op://test/mali-rsyncnet-reconciler/private key";
    known-hosts = "op://test/mali-rsyncnet-reconciler/known hosts";
  };
assert
  serviceConfig.LoadCredential == [
    "identity:${provider.socketPath}"
    "known-hosts:${provider.socketPath}"
  ];
assert builtins.elem "onepassword-credential-provider.socket" service.requires;
assert builtins.elem "onepassword-credential-provider.socket" service.after;
assert builtins.elem "${pkgs.coreutils}/bin/test -s %d/identity" serviceConfig.ExecStartPre;
assert builtins.elem "${pkgs.coreutils}/bin/test -s %d/known-hosts" serviceConfig.ExecStartPre;
assert service.wantedBy == [ ];
assert timer.wantedBy == [ "timers.target" ];
assert unscheduledTimer.wantedBy == [ ];
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
