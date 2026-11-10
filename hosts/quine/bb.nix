{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  username = config.modules.users.primaryUser.username;
  home = config.users.users.${username}.home;
  stateRoot = "${home}/.config/bb";
  serverUrl = "https://quine.${config.repo.secrets.global.domain.tailnet}:8444";
  agents = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
  package = agents.bb-app;
  cli = pkgs.writeShellScriptBin "bb-cli" ''
    export BB_DATA_DIR=${lib.escapeShellArg "${stateRoot}/server"}
    exec ${lib.getExe' package "bb"} "$@"
  '';
  desktop = pkgs.symlinkJoin {
    name = "bb-desktop-quine";
    paths = [ pkgs.bb-desktop ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram "$out/bin/bb-desktop" \
        --set BB_DATA_DIR ${lib.escapeShellArg "${stateRoot}/desktop-runtime"} \
        --add-flags ${lib.escapeShellArg "--user-data-dir=${stateRoot}/desktop"}
    '';
  };
in
{
  systemd.tmpfiles.settings."10-bb".${stateRoot}.d = {
    mode = "0700";
    user = username;
    group = config.users.users.${username}.group;
  };

  services.bb = {
    enable = true;
    user = username;
    dataDir = "${stateRoot}/server";
    inherit package;
    providers = {
      claude.enable = true;
      codex.enable = true;
      pi.enable = true;
    };
    extraPackages = [ pkgs.chromium ];
    environment = {
      BB_APP_URL = serverUrl;
      BB_DATA_DIR = "${stateRoot}/server";
      BB_TELEMETRY = "false";
      CLAUDE_CONFIG_DIR = "${home}/.config/claude";
      CODEX_HOME = "${home}/.config/codex";
      PI_CODING_AGENT_DIR = "${home}/.config/pi/agent";
    };
  };

  services.bbIsolation = {
    gateway.enable = true;
    egress = {
      enable = true;
      user = username;
      gatewayAddress = config.site.hosts.quine.tailscaleIPv4;
    };
  };

  # Foreground Serve owns only this port and withdraws it on service stop.
  # Keep the backend on loopback; tailnet ACLs control remote access.
  systemd.services.bb-tailscale-serve = {
    description = "bb private Tailscale HTTPS listener";
    wantedBy = [ "multi-user.target" ];
    after = [
      "tailscaled.service"
      "bb.service"
    ];
    requires = [ "tailscaled.service" ];
    wants = [ "bb.service" ];
    serviceConfig = {
      ExecStart = "${lib.getExe config.services.tailscale.package} serve --https=8444 http://127.0.0.1:${toString config.services.bb.port}";
      Restart = "on-failure";
      RestartSec = 5;
    };
  };

  myhm = hm: {
    home.packages = [
      cli
      desktop
    ];
    # Keep this mutable for the desktop's server menu, but select our server
    # on activation so launching the client does not create a second server.
    home.activation.bbDesktopSettings = hm.lib.hm.dag.entryAfter [ "linkGeneration" ] (
      hm.lib.hm.generators.mkImpureConfigMerger {
        inherit pkgs;
        format = "json";
        empty = "{}";
        jqOperation = "$dynamic * $static";
        path = "${stateRoot}/desktop/server-target.json";
        mode = "600";
        staticSettings = (pkgs.formats.json { }).generate "bb-server-target" {
          target = "custom";
          customServerUrl = "http://127.0.0.1:${toString config.services.bb.port}";
          showBuiltinServer = false;
        };
      }
    );
  };
}
