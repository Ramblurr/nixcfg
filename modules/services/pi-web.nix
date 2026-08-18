{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.services.pi-web;
  primaryUser = config.modules.users.primaryUser;

  inherit (lib)
    concatStringsSep
    getExe
    getExe'
    mkEnableOption
    mkIf
    mkOption
    mkPackageOption
    optionalAttrs
    types
    ;

  userRecord = config.users.users.${cfg.user} or { };
  homeDirectory =
    userRecord.home
      or (if cfg.user == primaryUser.username then primaryUser.homeDirectory else "/home/${cfg.user}");
  userShell = userRecord.shell or primaryUser.shell or pkgs.bashInteractive;
  shellPath = if lib.isDerivation userShell then getExe userShell else toString userShell;
  userUid = userRecord.uid or null;

  serviceName = "pi-web";
  sessiondServiceName = "pi-web-sessiond";
  runtimeDirectory = serviceName;
  sessiondSocket = "/run/${runtimeDirectory}/sessiond.sock";

  # PI WEB spawns shells and agents; include the user's profile so the
  # modules.dev.llms Pi package is visible to the system services.
  binPath = concatStringsSep ":" (
    [
      "${cfg.npmGlobalPrefix}/bin"
      "${homeDirectory}/.local/bin"
      "/etc/profiles/per-user/${cfg.user}/bin"
      "/run/current-system/sw/bin"
      (lib.makeBinPath cfg.extraPackages)
    ]
    ++ cfg.extraBinPaths
  );

  allowedHostsEnvironment = optionalAttrs (cfg.allowedHosts != null) {
    PI_WEB_ALLOWED_HOSTS =
      if cfg.allowedHosts == true then "true" else concatStringsSep "," cfg.allowedHosts;
  };

  xdgEnvironment = {
    HOME = homeDirectory;
    SHELL = shellPath;
    XDG_CONFIG_HOME = "${homeDirectory}/.config";
    XDG_DATA_HOME = "${homeDirectory}/.local/share";
    XDG_CACHE_HOME = "${homeDirectory}/.cache";
    NPM_CONFIG_PREFIX = toString cfg.npmGlobalPrefix;
    NPM_CONFIG_CACHE = "${cfg.cacheDir}/npm";
    PI_CONFIG_DIR = "${homeDirectory}/.config/pi";
    PI_CODING_AGENT_DIR = "${homeDirectory}/.config/pi/agent";
    PI_WEB_CONFIG = "${cfg.configDir}/config.json";
    PI_WEB_DATA_DIR = toString cfg.dataDir;
    PI_WEB_SESSIOND_SOCKET = sessiondSocket;
    NODE_ENV = "production";
    PATH = lib.mkForce binPath;
  }
  // optionalAttrs (userUid != null) {
    XDG_RUNTIME_DIR = "/run/user/${toString userUid}";
    DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/${toString userUid}/bus";
  };

  webEnvironment =
    xdgEnvironment
    // allowedHostsEnvironment
    // {
      PI_WEB_HOST = cfg.host;
      PI_WEB_PORT = toString cfg.ports.http;
    };

  upstreamHost = if cfg.host == "0.0.0.0" then "127.0.0.1" else cfg.host;
in
{
  options.modules.services.pi-web = {
    enable = mkEnableOption "PI WEB, a web control plane for Pi Coding Agent sessions";

    package = mkPackageOption pkgs "pi-web" { };

    user = mkOption {
      type = types.str;
      default = primaryUser.username;
      description = "User to run PI WEB as.";
    };

    group = mkOption {
      type = types.str;
      default = primaryUser.username;
      description = "Group to run PI WEB as.";
    };

    host = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Address for the PI WEB web server to bind.";
    };

    ports.http = mkOption {
      type = types.port;
      default = 8504;
      description = "Port for the PI WEB web server.";
    };

    allowedHosts = mkOption {
      type = types.nullOr (types.either (types.enum [ true ]) (types.listOf types.str));
      default = null;
      description = ''
        Optional value for PI_WEB_ALLOWED_HOSTS. Use true to allow any Host header,
        or a list of accepted hostnames.
      '';
    };

    configDir = mkOption {
      type = types.path;
      default = "${homeDirectory}/.config/pi-web";
      description = "XDG configuration directory for PI WEB.";
    };

    dataDir = mkOption {
      type = types.path;
      default = "${homeDirectory}/.local/share/pi-web";
      description = "XDG data directory for PI WEB projects, machines, plugins, and archives.";
    };

    cacheDir = mkOption {
      type = types.path;
      default = "${homeDirectory}/.cache/pi-web";
      description = "XDG cache directory for PI WEB and npm.";
    };

    npmGlobalPrefix = mkOption {
      type = types.path;
      default = "${homeDirectory}/.local/share/npm";
      description = "Writable npm global prefix used by Pi extensions and globally installed tools.";
    };

    extraPackages = mkOption {
      type = types.listOf types.package;
      default = [
        pkgs.bashInteractive
        pkgs.coreutils
        pkgs.git
        pkgs.openssh
      ];
      description = "Packages added to PATH for PI WEB services and spawned terminals.";
    };

    extraBinPaths = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Additional raw PATH entries for PI WEB services and spawned terminals.";
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = "Open the HTTP port in the host firewall.";
    };

    domain = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "pi-web.example.com";
      description = "Optional ingress virtual host domain for PI WEB.";
    };

  };

  config = mkIf cfg.enable {

    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [ cfg.ports.http ];

    systemd.tmpfiles.rules = [
      "d ${cfg.configDir} 0700 ${cfg.user} ${cfg.group} - -"
      "d ${cfg.dataDir} 0700 ${cfg.user} ${cfg.group} - -"
      "d ${cfg.cacheDir} 0700 ${cfg.user} ${cfg.group} - -"
      "d ${cfg.cacheDir}/npm 0700 ${cfg.user} ${cfg.group} - -"
    ];

    systemd.services.${sessiondServiceName} = {
      description = "PI WEB session daemon";
      documentation = [ "https://pi-web.dev/" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];
      environment = xdgEnvironment;
      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.group;
        WorkingDirectory = homeDirectory;
        ExecStart = getExe' cfg.package "pi-web-sessiond";
        Restart = "on-failure";
        RestartSec = "5s";
        RuntimeDirectory = runtimeDirectory;
        RuntimeDirectoryMode = "0700";
        UMask = "0022";
      };
    };

    systemd.services.${serviceName} = {
      description = "PI WEB web server";
      documentation = [ "https://pi-web.dev/" ];
      after = [
        "network-online.target"
        "${sessiondServiceName}.service"
      ];
      wants = [ "network-online.target" ];
      requires = [ "${sessiondServiceName}.service" ];
      wantedBy = [ "multi-user.target" ];
      environment = webEnvironment;
      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.group;
        WorkingDirectory = homeDirectory;
        ExecStart = getExe' cfg.package "pi-web-server";
        Restart = "on-failure";
        RestartSec = "5s";
        UMask = "0022";
      };
    };

    modules.services.caddy.protectedRoutes.pi-web = mkIf (cfg.domain != null) {
      publicHost = cfg.domain;
      upstream = "http://${upstreamHost}:${toString cfg.ports.http}";
    };

    ## pi-web tailscale proxy
    #systemd.sockets.pi-web-tailscale-proxy = {
    #  description = "PI WEB proxy socket on external addresses";
    #  wantedBy = [ "sockets.target" ];
    #  listenStreams = [
    #    "${tailscaleIp4}:${toString config.modules.services.pi-web.ports.http}"
    #    "${primAddress}:${toString config.modules.services.pi-web.ports.http}"
    #  ];
    #  socketConfig = {
    #    FreeBind = true;
    #    NoDelay = true;
    #  };
    #};
    #systemd.services.pi-web-tailscale-proxy = {
    #  description = "PI WEB external address proxy";
    #  requires = [
    #    "pi-web.service"
    #    "pi-web-tailscale-proxy.socket"
    #  ];
    #  after = [
    #    "pi-web.service"
    #    "pi-web-tailscale-proxy.socket"
    #  ];
    #  serviceConfig = {
    #    ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd 127.0.0.1:${toString config.modules.services.pi-web.ports.http}";
    #    DynamicUser = true;
    #    PrivateTmp = true;
    #    ProtectHome = true;
    #    ProtectSystem = "strict";
    #  };
    #};

  };
}
