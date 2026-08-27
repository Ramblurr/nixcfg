{ inputs, pkgs }:
let
  lib = inputs.nixpkgs.lib;
  groups = import ../modules/site/gatus-groups.nix;
  secretFile = pkgs.writeText "gatus-heartbeats-test-secrets.yaml" "{}\n";
  heartbeatPackage = pkgs.callPackage ../pkgs/gatus-heartbeat.nix { };
  evaluate =
    enable: interval: environmentFile: dynamicUser:
    (lib.nixosSystem {
      modules = [
        inputs.sops-nix.nixosModules.sops
        ../modules/services/onepassword-systemd-credentials.nix
        ../modules/site/gatus.nix
        ../modules/site/gatus-heartbeats.nix
        ../modules/site/gatus-heartbeats-onepassword.nix
        {
          options = {
            repo.secrets = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
            site.net = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
          };
        }
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName = "dewey";
          system.stateVersion = "26.05";
          boot.loader.grub.devices = [ "nodev" ];
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
          sops.defaultSopsFile = secretFile;
          sops.age.keyFile = "/tmp/age-key.txt";
          site.net.mgmt.hosts4.onepassword-connect = [ "192.0.2.22" ];
          repo.secrets.global.domain.home = "example.test";
          site.gatus.heartbeatToken = {
            inherit environmentFile;
            gatusEnvironmentVariable =
              if environmentFile == null then "GATUS_EXTERNAL_TOKEN" else "GATUS_QUINE_EXTERNAL_TOKEN";
          };
          modules.services.onepassword-systemd-credentials.enable = environmentFile == null;
          systemd.services.example-job.serviceConfig = {
            Type = "oneshot";
            DynamicUser = dynamicUser;
            ExecStartPost = [ "${pkgs.coreutils}/bin/true" ];
          };
          site.gatus.heartbeats.git-archive = lib.mkIf enable {
            service = "example-job";
            name = "Git Archive";
            group = groups.work;
            inherit interval;
          };
        }
      ];
    }).config;
  enabled = evaluate true "30h" null true;
  staticUserEnabled = evaluate true "30h" null false;
  disabled = evaluate false "30h" null false;
  environmentFileEnabled = evaluate true "30h" "/run/secrets/gatus-heartbeat" false;
  invalidInterval = builtins.tryEval (
    builtins.deepSeq (evaluate true "8d" null false).site.gatus.externalEndpoints true
  );
  reporterCommand = lib.last enabled.systemd.services.example-job.serviceConfig.ExecStartPost;
  staticUserReporterCommand = lib.last staticUserEnabled.systemd.services.example-job.serviceConfig.ExecStartPost;
  environmentFileReporterCommand = lib.last environmentFileEnabled.systemd.services.example-job.serviceConfig.ExecStartPost;
in
assert !invalidInterval.success;
assert enabled.site.gatus.groups == groups;
assert enabled.site.gatus.heartbeatToken.available;
assert environmentFileEnabled.site.gatus.heartbeatToken.available;
assert environmentFileEnabled.modules.services.onepassword-systemd-credentials.consumers == { };
assert
  (builtins.head environmentFileEnabled.site.gatus.externalEndpoints).token
  == "$GATUS_QUINE_EXTERNAL_TOKEN";
assert
  enabled.site.gatus.externalEndpoints == [
    {
      name = "Git Archive (dewey)";
      group = groups.work;
      token = "$GATUS_EXTERNAL_TOKEN";
      heartbeat.interval = "30h";
      alerts = [ { type = "pushover"; } ];
    }
  ];
assert
  enabled.modules.services.onepassword-systemd-credentials.consumers.example-job.gatus-token
  == "op://home-ops-prod/gatus/borgmatic_external_endpoint_token";
assert
  builtins.head enabled.systemd.services.example-job.serviceConfig.ExecStartPost
  == "${pkgs.coreutils}/bin/true";
assert !(lib.hasPrefix "+" reporterCommand);
assert reporterCommand == staticUserReporterCommand;
assert (enabled.systemd.services.example-job.serviceConfig.ExecStopPost or null) == null;
assert lib.hasInfix "gatus-heartbeat report" reporterCommand;
assert lib.hasInfix "--success true" reporterCommand;
assert lib.hasInfix "--group '${groups.work}'" reporterCommand;
assert lib.hasInfix "--name 'Git Archive (dewey)'" reporterCommand;
assert lib.hasInfix "--token-file %d/gatus-token" reporterCommand;
assert !(lib.hasInfix "--token-file" environmentFileReporterCommand);
assert
  environmentFileEnabled.systemd.services.example-job.serviceConfig.EnvironmentFile
  == "/run/secrets/gatus-heartbeat";
assert disabled.site.gatus.externalEndpoints == [ ];
pkgs.runCommand "gatus-heartbeats-test"
  {
    nativeBuildInputs = [
      heartbeatPackage
      pkgs.python3
    ];
  }
  ''
      export REQUEST_LOG="$TMPDIR/request.json"
      cat > "$TMPDIR/server.py" <<'PY'
    import json
    import os
    from http.server import BaseHTTPRequestHandler, HTTPServer

    class Handler(BaseHTTPRequestHandler):
        def do_POST(self):
            with open(os.environ["REQUEST_LOG"], "w", encoding="utf-8") as output:
                json.dump({"path": self.path, "authorization": self.headers.get("Authorization")}, output)
            self.send_response(200)
            self.end_headers()

        def log_message(self, *_args):
            pass

    server = HTTPServer(("127.0.0.1", 18080), Handler)
    server.timeout = 10
    server.handle_request()
    PY
      python "$TMPDIR/server.py" &
      server_pid=$!
      sleep 0.2

      export GATUS_EXTERNAL_TOKEN=test-token
      gatus-heartbeat report \
        --url http://127.0.0.1:18080 \
        --group "${groups.work}" \
        --name "Git Archive (dewey)" \
        --success true
      wait "$server_pid"

      python - "$REQUEST_LOG" <<'PY'
    import json
    import sys
    from urllib.parse import parse_qs, urlparse

    with open(sys.argv[1], encoding="utf-8") as request_file:
        request = json.load(request_file)
    parsed = urlparse(request["path"])
    assert parsed.path == "/api/v1/endpoints/work---collaboration_git-archive-(dewey)/external"
    assert parse_qs(parsed.query) == {"success": ["true"]}
    assert request["authorization"] == "Bearer test-token"
    PY
      touch "$out"
  ''
