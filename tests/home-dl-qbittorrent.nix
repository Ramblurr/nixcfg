{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  hostPkgs = import inputs.nixpkgs {
    system = pkgs.stdenv.hostPlatform.system;
    config.allowUnfree = true;
    overlays = [ (import ../overlays/nixpkgs-mine-packages.nix inputs) ];
  };
  fixtureModule = { lib, ... }: {
    options = {
      modules.services.home-dl = {
        enable = lib.mkEnableOption "home-dl test fixture";
        baseDomain = lib.mkOption { type = lib.types.str; };
      };
      modules.services.caddy.routes = lib.mkOption {
        type = lib.types.attrsOf lib.types.attrs;
        default = { };
      };
      repo.secrets = lib.mkOption { type = lib.types.unspecified; };
      site.net.mgmt.hosts4.onepassword-connect = lib.mkOption {
        type = lib.types.listOf lib.types.str;
      };
      site.gatus.endpoints = lib.mkOption {
        type = lib.types.listOf lib.types.attrs;
        default = [ ];
      };
    };
  };
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      inputs.sops-nix.nixosModules.sops
      inputs.vpn-confinement.nixosModules.default
      ../modules/services/onepassword-systemd-credentials.nix
      ../modules/services/home-dl/qbittorrent.nix
      fixtureModule
      {
        nixpkgs.pkgs = hostPkgs;
        networking.hostName = "dewey";
        boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = true;
        system.stateVersion = "26.05";
        modules.services.home-dl = {
          enable = true;
          baseDomain = "example.test";
          ports.qbittorrent = 10019;
        };
        modules.services.onepassword-systemd-credentials = {
          enable = true;
          bootstrapTokenFile = "/run/test-onepassword-token";
          package = pkgs.writeShellScriptBin "op" "exit 1";
        };
        repo.secrets = {
          home-ops = {
            users = {
              media.name = "media";
              qui = {
                name = "qui";
                uid = 3023;
                isSystemUser = true;
              };
            };
            groups = {
              media.name = "media";
              qui = {
                name = "qui";
                gid = 3023;
              };
            };
          };
        };
        users.users.media = {
          isSystemUser = true;
          group = "media";
        };
        users.groups.media = { };
        site.net.mgmt.hosts4.onepassword-connect = [ "192.0.2.22" ];
      }
    ];
  };
  inherit (evaluated) config;
  namespace = config.vpnNamespaces.qbtvpn;
  qbittorrent = config.services.qbittorrent;
  qbittorrentReadOnlyBinds = config.systemd.services.qbittorrent.serviceConfig.BindReadOnlyPaths;
  watchedFoldersBind =
    lib.findFirst (lib.hasSuffix ":/var/lib/qbittorrent/qBittorrent/config/watched_folders.json") null
      qbittorrentReadOnlyBinds;
  watchedFoldersConfig = lib.head (lib.splitString ":" watchedFoldersBind);
  qui = config.services.qui;
  portForward = config.systemd.services.proton-qbittorrent-port-forward;
  credentialProvider = config.modules.services.onepassword-systemd-credentials;
  parentWithoutImport = lib.replaceStrings [ "./home-dl/qbittorrent.nix" ] [ "" ] (
    builtins.readFile ../modules/services/home-dl.nix
  );

  fakeCommands = pkgs.runCommand "proton-port-forward-fakes" { } ''
    mkdir -p $out/bin
    cat > $out/bin/ip <<'SH'
    #!${pkgs.runtimeShell}
    printf '%s\n' '10.2.0.1 dev qbtvpn0 src 10.2.0.2'
    SH
    cat > $out/bin/iptables <<'SH'
    #!${pkgs.runtimeShell}
    printf 'iptables %s\n' "$*" >> "$EVENTS"
    SH
    cat > $out/bin/curl <<'SH'
    #!${pkgs.runtimeShell}
    printf 'curl %s\n' "$*" >> "$EVENTS"
    case "''${FAKE_MODE:-success}:$*" in
      api-fail:*listen_port*:*qbtvpn0*) exit 1 ;;
    esac
    SH
    cat > $out/bin/natpmpc <<'SH'
    #!${pkgs.runtimeShell}
    printf 'natpmpc %s\n' "$*" >> "$EVENTS"
    public_port=$4
    protocol=$6
    lifetime=$7
    [ "$lifetime" = 0 ] && exit 0

    count_file="$TEST_ROOT/natpmp-count"
    count=0
    [ ! -e "$count_file" ] || count=$(cat "$count_file")
    count=$((count + 1))
    printf '%s\n' "$count" > "$count_file"

    assigned_port=$public_port
    [ "$public_port" != 1 ] || assigned_port=45678
    case "''${FAKE_MODE:-success}:$count" in
      mismatch:2|renew-change:3) assigned_port=45679 ;;
      failure:1) exit 1 ;;
      bad-lifetime:1)
        printf '%s\n' 'Mapped public port 45678 protocol UDP to local port 0 lifetime 30'
        exit 0
        ;;
      malformed:1)
        printf '%s\n' 'Mapped public port 45678 protocol UDP to local port 0 lifetime 60'
        printf '%s\n' 'Mapped public port 45678 protocol UDP to local port 0 lifetime 60'
        exit 0
        ;;
    esac
    protocol=$(printf '%s' "$protocol" | tr '[:lower:]' '[:upper:]')
    printf 'Mapped public port %s protocol %s to local port 0 lifetime 60\n' "$assigned_port" "$protocol"
    SH
    cat > $out/bin/sleep <<'SH'
    #!${pkgs.runtimeShell}
    count_file="$TEST_ROOT/sleep-count"
    count=0
    [ ! -e "$count_file" ] || count=$(cat "$count_file")
    if [ "$count" -ge "''${MAX_SLEEPS:-0}" ]; then
      kill -TERM "$PPID"
      exit 1
    fi
    printf '%s\n' "$((count + 1))" > "$count_file"
    SH
    chmod +x $out/bin/*
  '';
  runScenario = pkgs.writeShellScript "run-port-forward-scenario" ''
    name=$1
    mode=$2
    max_sleeps=$3
    export TEST_ROOT="$PWD/$name"
    export EVENTS="$TEST_ROOT/events"
    export RUNTIME_DIRECTORY="$TEST_ROOT/runtime"
    export FAKE_MODE=$mode
    export MAX_SLEEPS=$max_sleeps
    mkdir -p "$RUNTIME_DIRECTORY"
    PATH=${fakeCommands}/bin:${pkgs.coreutils}/bin:${pkgs.gnugrep}/bin \
      ${portForward.serviceConfig.ExecStart}
  '';
in
assert config.boot.kernel.sysctl."net.ipv6.conf.all.forwarding" == 1;
assert lib.versionAtLeast qbittorrent.package.version "5.2.3";
assert qbittorrent.profileDir == "/var/lib/qbittorrent";
assert
  config.systemd.services.qbittorrent.serviceConfig.BindPaths == [
    "/var/lib/private/home-dl/qbittorrent:/var/lib/qbittorrent"
  ];
assert watchedFoldersBind != null;
assert
  config.systemd.services.qbittorrent.serviceConfig.ReadWritePaths == [
    "/var/lib/qbittorrent"
    "/mnt/downloads"
  ];
assert qbittorrent.webuiPort == 8085;
assert !qbittorrent.openFirewall;
assert qbittorrent.serverConfig.LegalNotice.Accepted;
assert
  qbittorrent.serverConfig.Preferences.Connection == {
    Interface = "lo";
    PortRangeMin = 0;
    RandomPort = false;
    UPnP = false;
  };
assert
  qbittorrent.serverConfig.BitTorrent.Session == {
    DHTEnabled = false;
    FinishedTorrentExportDirectory = "/mnt/downloads/torrents/qbit/torrents-complete";
    LSDEnabled = false;
    PeXEnabled = false;
    TorrentExportDirectory = "/mnt/downloads/torrents/qbit/torrents";
    TorrentContentLayout = "Subfolder";
  };
assert
  qbittorrent.serverConfig.Preferences.Downloads == {
    SavePath = "/mnt/downloads/torrents/qbit/complete/";
    TempPath = "/mnt/downloads/torrents/qbit/incomplete/";
    TempPathEnabled = true;
  };
assert
  qbittorrent.serverConfig.Preferences.Queueing == {
    MaxActiveDownloads = 10;
    MaxActiveTorrents = 100;
    MaxActiveUploads = 90;
    QueueingEnabled = true;
  };
assert
  config.systemd.services.qbittorrent.vpnConfinement == {
    enable = true;
    vpnNamespace = "qbtvpn";
  };
assert lib.elem "qbtvpn.service" config.systemd.services.qbittorrent.bindsTo;
assert !config.systemd.services.qui.vpnConfinement.enable;
assert
  {
    inherit (qui) user;
    inherit (qui) group;
    uid = config.users.users.qui.uid;
    gid = config.users.groups.qui.gid;
  } == {
    user = "qui";
    group = "qui";
    uid = 3023;
    gid = 3023;
  };
assert
  qui.settings == {
    host = "127.0.0.1";
    oidcClientId = "eaff8d42-7b05-48b3-9471-df0b2a0165ab";
    oidcDisableBuiltInLogin = false;
    oidcEnabled = true;
    oidcIssuer = "https://id.example.test";
    oidcRedirectUrl = "https://qbittorrent.example.test/api/auth/oidc/callback";
    port = 10019;
  };
assert !qui.openFirewall;
assert
  config.systemd.services.qui.serviceConfig.LoadCredential == [
    "oidcClientSecret:${credentialProvider.socketPath}"
    "sessionSecret:${credentialProvider.socketPath}"
  ];
assert
  config.systemd.services.qui.environment.QUI__OIDC_CLIENT_SECRET_FILE
  == credentialProvider.creds.qui.oidcClientSecret;
assert namespace.wireguardConfigFile == "/run/qbtvpn/wireguard.conf";
assert lib.hasInfix "Address = 10.2.0.2/32" config.systemd.services.qbtvpn.preStart;
assert lib.hasInfix "DNS = 10.2.0.1" config.systemd.services.qbtvpn.preStart;
assert lib.hasInfix "AllowedIPs = 0.0.0.0/0" config.systemd.services.qbtvpn.preStart;
assert namespace.accessibleFrom == [ "192.168.15.5" ];
assert
  namespace.portMappings == [
    {
      from = 8085;
      protocol = "tcp";
      to = 8085;
    }
  ];
assert
  portForward.vpnConfinement == {
    enable = true;
    vpnNamespace = "qbtvpn";
  };
assert lib.elem "qbtvpn.service" portForward.bindsTo;
assert lib.elem "qbittorrent.service" portForward.after;
assert
  config.modules.services.onepassword-systemd-credentials.consumers == {
    qbtvpn = {
      privateKey = "op://home-ops-prod/protonvpn-dewey/PrivateKey";
      peerPublicKey = "op://home-ops-prod/protonvpn-dewey/PeerPublicKey";
      peerEndpoint = "op://home-ops-prod/protonvpn-dewey/PeerEndpoint";
    };
    qui = {
      oidcClientSecret = "op://home-ops-prod/qbittorrent/oidc-client-secret";
      sessionSecret = "op://home-ops-prod/qbittorrent/qui-session-secret";
    };
  };
assert config.modules.services.caddy.routes.qbittorrent.upstream == "http://127.0.0.1:10019";
assert !(lib.hasInfix "gluetun" (lib.toLower parentWithoutImport));
pkgs.runCommand "home-dl-qbittorrent" { } ''
  ${pkgs.jq}/bin/jq -e '
    .["/mnt/downloads/torrents/qbit/blackhole"] == {
      "add_torrent_params": {
        "content_layout": "Subfolder",
        "save_path": "/mnt/downloads/torrents/qbit/complete",
        "use_auto_tmm": false
      },
      "recursive": false
    }
  ' ${watchedFoldersConfig} >/dev/null
  ${runScenario} success success 1
  grep -F 'natpmpc -g 10.2.0.1 -a 1 0 udp 60' success/events
  grep -F 'natpmpc -g 10.2.0.1 -a 45678 0 tcp 60' success/events
  test "$(grep -c -- '-a 45678 0 udp 60' success/events)" -ge 1
  grep -F 'iptables -w -A qbt-pf -p tcp --dport 45678 -j ACCEPT' success/events
  grep -F 'iptables -w -A qbt-pf -p udp --dport 45678 -j ACCEPT' success/events
  grep -F 'current_network_interface%22%3A%22qbtvpn0' success/events || grep -F 'current_network_interface":"qbtvpn0' success/events
  grep -F 'natpmpc -g 10.2.0.1 -a 45678 0 udp 0' success/events
  grep -F 'natpmpc -g 10.2.0.1 -a 45678 0 tcp 0' success/events

  ${runScenario} mismatch mismatch 0
  ! grep -F -- '-A qbt-pf' mismatch/events
  grep -F 'natpmpc -g 10.2.0.1 -a 45678 0 udp 0' mismatch/events
  grep -F 'natpmpc -g 10.2.0.1 -a 45679 0 tcp 0' mismatch/events

  ${runScenario} malformed malformed 0
  ! grep -F -- '-A qbt-pf' malformed/events

  ${runScenario} failure failure 0
  ! grep -F -- '-A qbt-pf' failure/events

  ${runScenario} bad-lifetime bad-lifetime 0
  ! grep -F -- '-A qbt-pf' bad-lifetime/events

  ${runScenario} renew-change renew-change 1
  test "$(grep -c -- '-A qbt-pf' renew-change/events)" -eq 2
  grep -F 'natpmpc -g 10.2.0.1 -a 45678 0 udp 0' renew-change/events
  grep -F 'natpmpc -g 10.2.0.1 -a 45679 0 udp 0' renew-change/events

  ${runScenario} api-fail api-fail 0
  grep -F -- '-A qbt-pf' api-fail/events
  grep -F 'iptables -w -D INPUT -i qbtvpn0 -j qbt-pf' api-fail/events
  grep -F 'natpmpc -g 10.2.0.1 -a 45678 0 tcp 0' api-fail/events

  touch $out
''
