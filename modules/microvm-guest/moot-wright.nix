{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.microvm-guest.mootWright;
  inherit (cfg) name;
  inherit (config.repo.secrets.global.domain) tailnet;
  hostSecretsMount = config.modules.microvm-guest.hostSecrets.mountPoint;
  agents = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
  models = import ../dev/llms/models.nix;
  json = pkgs.formats.json { };
  ipv4 = import ../../lib/ipv4.nix { inherit lib; };

  uid = 2001;
  tsUid = 2101;
  home = "/home/${name}";
  configHome = "${home}/.config";
  netns = "moot-${name}";
  nsUnits = [
    "systemd-netns-private-${netns}.service"
    "systemd-netns-private-access-${netns}.service"
  ];
  veth = {
    hostIface = "mw-host";
    nsIface = "mw-ns";
    hostAddr = "192.168.254.1/30";
    nsAddr = "192.168.254.2/30";
  };
  uplink = "uplink";
  mac = lib.my.generateMacAddress "1-moot-${name}";
  nonPublic = "{ ${lib.concatStringsSep ", " (import ../../config/non-public-ipv4.nix)} }";

  proxyPort = 1056;
  t3Port = 3780;
  transparentAddr = "127.0.0.1:15001";
  captureMark = "0x1";
  captureTable = "100";
  keyFile = "/run/moot-wright/cliproxy-key";
  pluginsStateDir = "${home}/.local/share/ol-plugins-sync";
  t3Home = "${home}/.t3";
  dirs = {
    claude = "${configHome}/claude";
    codex = "${configHome}/codex";
    pi = "${configHome}/pi/agent";
    piT3 = "${configHome}/pi-t3/agent";
  };

  proxyEnv =
    let
      proxy = "http://127.0.0.1:${toString proxyPort}";
      noProxy = "localhost,127.0.0.1,::1";
    in
    {
      HTTPS_PROXY = proxy;
      HTTP_PROXY = proxy;
      https_proxy = proxy;
      http_proxy = proxy;
      NO_PROXY = noProxy;
      no_proxy = noProxy;
    };
  # The egress signs decrypted flows with its own CA. Node keeps its own
  # roots, so it needs the CA named on top of the system bundle.
  egressCa = ../../config/ts-egress-ca.pem;
  caEnv = {
    SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt";
    NODE_EXTRA_CA_CERTS = "${egressCa}";
  };
  wrightEnv =
    proxyEnv
    // caEnv
    // lib.genAttrs cfg.credentials (n: "TS_EGRESS_PLACEHOLDER_${n}")
    // {
      # Node's fetch ignores the proxy variables without this, and tailnet
      # names resolve only through the proxy.
      NODE_USE_ENV_PROXY = "1";
      XDG_RUNTIME_DIR = "/run/user/${toString uid}";
      DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/${toString uid}/bus";
      MOOT_MATRIX_ID = cfg.identity.matrixId;
      CLAUDE_CONFIG_DIR = dirs.claude;
      CODEX_HOME = dirs.codex;
      PI_CODING_AGENT_DIR = dirs.pi;
      T3CODE_HOME = t3Home;
      CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC = "1";
      DISABLE_AUTOUPDATER = "1";
      DISABLE_TELEMETRY = "1";
    };
  exports =
    env:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "export ${k}=${lib.escapeShellArg v}") env);

  withKey =
    bin: package: variable: extraEnv:
    pkgs.writeShellScriptBin bin ''
      ${variable}="$(< ${keyFile})"
      export ${variable}
      ${exports extraEnv}
      exec ${lib.getExe package} "$@"
    '';
  claude = withKey "claude" agents.claude-code "ANTHROPIC_AUTH_TOKEN" {
    ANTHROPIC_BASE_URL = cfg.cliproxy.url;
  };
  codex = withKey "codex" agents.codex "CLIPROXY_API_KEY" { };
  pi = agents.pi.override { useBun = false; };
  antigravity = agents.antigravity-cli;
  t3 = pkgs.t3code.override {
    providerPackages = [
      claude
      codex
      pi
    ];
  };
  providers = [
    claude
    codex
    pi
    antigravity
  ];

  claudeSettings = {
    model = models.defaults.claude;
    includeCoAuthoredBy = false;
    # Claude Code only picks the 1h cache TTL itself on direct subscription auth.
    promptCacheTtl = "1h";
    env.ANTHROPIC_BASE_URL = cfg.cliproxy.url;
  };
  codexSettings = {
    model = models.defaults.codex;
    model_provider = "cliproxy";
    model_providers.cliproxy = {
      name = "CLIProxyAPI";
      base_url = "${cfg.cliproxy.url}/v1";
      wire_api = "responses";
      requires_openai_auth = false;
      env_key = "CLIPROXY_API_KEY";
    };
  };
  piDefaults = {
    defaultProvider = "cliproxy";
    defaultModel = models.defaults.pi;
    defaultThinkingLevel = "high";
  };
  piPackages = profile: { packages = [ "${pluginsStateDir}/profiles/${profile}" ]; };
  piModels.providers.cliproxy = import ../dev/llms/cliproxy-pi-provider.nix {
    inherit (cfg.cliproxy) url;
    apiKey = "!cat ${keyFile}";
  };
  t3Settings = {
    linkedEnvironmentsEnabled = true;
    enableProviderUpdateChecks = false;
    providerInstances = {
      claudeAgent = {
        driver = "claudeAgent";
        config.homePath = dirs.claude;
      };
      codex = {
        driver = "codex";
        config.homePath = dirs.codex;
      };
      pi = {
        driver = "pi";
        environment = [
          {
            name = "PI_CODING_AGENT_DIR";
            value = dirs.piT3;
            sensitive = false;
          }
        ];
      };
      antigravity = {
        driver = "antigravity";
        config.binaryPath = lib.getExe antigravity;
      };
    };
  };
  t3Defaults.providerInstances =
    lib.mapAttrs (_: _: { enabled = true; }) t3Settings.providerInstances
    // {
      pi = {
        displayName = "Pi";
        enabled = true;
      };
    };
  t3Environment = ''
    .providerInstances.pi.environment = [
      ($user.providerInstances.pi.environment // [])[]
      | select(.name | IN($managed[0].providerInstances.pi.environment[].name) | not)
    ] + $managed[0].providerInstances.pi.environment
  '';
  pluginsSyncConfig = pkgs.writeText "ol-plugins-sync.edn" ''
    {:state-dir ${builtins.toJSON pluginsStateDir}
     :t3-settings ${builtins.toJSON "${t3Home}/userdata/settings.json"}
     :discover-t3 true
     :include-antigravity-global true
     :codex-bin ${builtins.toJSON (lib.getExe agents.codex)}
     :retain-revs 2
     :peers []
     :packs {"harness-plugins" {:url "https://git.outskirtslabs.com/casey/skills.git" :branch "main"}}
     :defaults {:plugins ["my@harness-plugins" "pstack@harness-plugins" "tstack@harness-plugins"]}
     :profiles {"pi-cli" {:harness :pi :dir ${builtins.toJSON dirs.pi}}}}
  '';

  json5 = pkgs.python3Packages.toPythonApplication pkgs.python3Packages.json5;
  jq = lib.getExe pkgs.jq;
  remarshal = lib.getExe' pkgs.remarshal "remarshal";
  # Paths are relative to the home directory: creating any component of the
  # /home mount fails on virtiofs, even when it exists.
  rel = lib.removePrefix "${home}/";
  mergeJson =
    absolute:
    {
      managed,
      defaults ? { },
      filter ? ".",
    }:
    let
      path = rel absolute;
    in
    ''
      install -d -m 0700 "$(dirname ${path})"
      [ -s ${path} ] || echo '{}' > ${path}
      ${lib.getExe json5} --as-json < ${path} \
        | ${jq} \
          --slurpfile defaults ${json.generate "defaults.json" defaults} \
          --slurpfile managed ${json.generate "managed.json" managed} \
          ${lib.escapeShellArg "($defaults[0] * .) as $user | $user * $managed[0] | ${filter}"} > ${path}.new
      mv ${path}.new ${path}
    '';
  mergeToml =
    absolute: settings:
    let
      path = rel absolute;
    in
    ''
      install -d -m 0700 "$(dirname ${path})"
      [ -s ${path} ] || : > ${path}
      ${remarshal} -if toml -of json < ${path} \
        | ${jq} --slurpfile static ${json.generate "config.json" settings} '. * $static[0]' \
        | ${remarshal} -if json -of toml > ${path}.new
      mv ${path}.new ${path}
    '';
  writeConfig = absolute: file: ''
    install -D -m 0644 ${file} ${rel absolute}
  '';
  configure = pkgs.writeShellScript "moot-wright-config" ''
    set -euo pipefail
    umask 077
    cd ${home}
    ${mergeJson "${dirs.claude}/settings.json" {
      managed = claudeSettings;
      filter = "del(.env.ANTHROPIC_AUTH_TOKEN, .env.ANTHROPIC_API_KEY)";
    }}
    ${mergeToml "${dirs.codex}/config.toml" codexSettings}
    ${mergeJson "${t3Home}/userdata/settings.json" {
      managed = t3Settings;
      defaults = t3Defaults;
      filter = t3Environment;
    }}
    ${lib.concatMapStrings
      (
        { dir, profile }:
        mergeJson "${dir}/settings.json" {
          managed = piPackages profile;
          defaults = piDefaults;
        }
        + mergeJson "${dir}/models.json" {
          managed = piModels;
          filter = ".providers.cliproxy = $managed[0].providers.cliproxy";
        }
      )
      [
        {
          dir = dirs.pi;
          profile = "pi-cli";
        }
        {
          dir = dirs.piT3;
          profile = "pi";
        }
      ]
    }
    ${writeConfig "${configHome}/ol-plugins-sync/config.edn" pluginsSyncConfig}
  '';

  captureRules = pkgs.writeText "moot-${name}-capture.nft" ''
    table inet moot-capture
    delete table inet moot-capture
    table inet moot-capture {
      chain output {
        type route hook output priority mangle; policy accept;
        meta skuid ${toString tsUid} return
        fib daddr type local return
        meta nfproto ipv6 reject
        meta l4proto { tcp, udp } meta mark set ${captureMark} return
        counter drop
      }
      chain prerouting {
        type filter hook prerouting priority mangle; policy accept;
        iifname != "lo" return
        meta mark ${captureMark} meta l4proto { tcp, udp } tproxy ip to ${transparentAddr} accept
        meta mark ${captureMark} meta l4proto tcp reject with tcp reset
        meta mark ${captureMark} drop
      }
      # A socket bound to ${veth.nsIface} skips the capture route. Output
      # chains see the device from before the reroute; postrouting sees the final one.
      chain postrouting {
        type filter hook postrouting priority filter; policy accept;
        oifname "lo" accept
        meta skuid ${toString tsUid} accept
        counter drop
      }
    }
  '';

  confinement = {
    bindsTo = [ "nftables.service" ] ++ nsUnits;
    after = [ "nftables.service" ] ++ nsUnits;
  };
  inNetns = {
    NetworkNamespacePath = "/var/run/netns/${netns}";
    BindReadOnlyPaths = [ "/etc/netns/${netns}/resolv.conf:/etc/resolv.conf:norbind" ];
  };
  credentialUnit = "moot-wright-credentials.service";
  configUnit = "moot-wright-config.service";
  builder = cfg.nix.remoteBuilder;
in
{
  imports = [ ../networking/systemd-netns-private.nix ];

  options.modules.microvm-guest.mootWright = {
    enable = lib.mkEnableOption "a confined Moot wright: one agent user whose traffic leaves only through its own tailnet node";

    name = lib.mkOption {
      type = lib.types.strMatching "[a-z][a-z0-9-]{0,15}";
      default = config.networking.hostName;
      defaultText = lib.literalExpression "config.networking.hostName";
      description = "The wright's name: its Unix user, tailnet hostname and identity.";
    };

    network = {
      tapId = lib.mkOption {
        type = lib.types.strMatching "[a-zA-Z0-9-]{1,15}";
        description = "Host tap interface name; must match the host's isolatedTaps entry.";
      };
      hostAddress = lib.mkOption {
        inherit (ipv4) type;
        description = "Host IPv4 address on the tap, used as the default gateway.";
      };
      guestAddress = lib.mkOption {
        inherit (ipv4) type;
        description = "Guest IPv4 address on the tap.";
      };
      prefixLength = lib.mkOption {
        type = lib.types.ints.between 1 32;
        default = 30;
        description = "Prefix length of the tap subnet.";
      };
      dnsResolver = lib.mkOption {
        type = lib.types.str;
        default = "1.1.1.1";
        description = "Public numeric DNS server for the guest and for ts-plug's lookups through the exit node.";
      };
    };

    tailscale = {
      hostname = lib.mkOption {
        type = lib.types.str;
        default = name;
        defaultText = lib.literalExpression "config.modules.microvm-guest.mootWright.name";
        description = "Tailnet hostname of the wright's ts-plug node.";
      };
      advertiseTags = lib.mkOption {
        type = lib.types.listOf (lib.types.strMatching "tag:[a-zA-Z0-9-]+");
        default = [ ];
        description = ''
          ACL tags ts-plug requests. Required when the auth key file holds an
          OAuth client secret (tskey-client-...); leave empty for a pre-tagged
          auth key (tskey-auth-...).
        '';
      };
      exitNode = lib.mkOption {
        type = lib.types.str;
        default = "ward";
        description = "Exit node carrying all of the wright's traffic.";
      };
      authKeyFile = lib.mkOption {
        type = lib.types.str;
        default = "${hostSecretsMount}/ts-authkey";
        defaultText = lib.literalExpression ''"''${hostSecrets.mountPoint}/ts-authkey"'';
        description = "Tailscale auth key or OAuth client secret, read only by ts-plug.";
      };
    };

    cliproxy = {
      url = lib.mkOption {
        type = lib.types.str;
        apply = lib.removeSuffix "/";
        default = "https://moot-llm.${tailnet}";
        defaultText = lib.literalExpression ''"https://moot-llm.''${tailnet}"'';
        description = "CLIProxyAPI root URL, without /v1.";
      };
      keyFile = lib.mkOption {
        type = lib.types.str;
        default = "${hostSecretsMount}/cliproxy-key";
        defaultText = lib.literalExpression ''"''${hostSecrets.mountPoint}/cliproxy-key"'';
        description = "CLIProxyAPI client key, copied to ${keyFile} for the wright.";
      };
    };

    nix = {
      maxJobs = lib.mkOption {
        type = lib.types.ints.unsigned;
        default = 0;
        description = "Local build jobs; 0 builds only on the remote builder.";
      };
      substituters = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Extra binary caches, fetched through ts-plug.";
      };
      trustedPublicKeys = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Signing keys for the extra binary caches.";
      };
      remoteBuilder = lib.mkOption {
        default = null;
        description = "Remote builder reached over ssh-ng through ts-plug.";
        type = lib.types.nullOr (
          lib.types.submodule {
            options = {
              hostName = lib.mkOption {
                type = lib.types.str;
                description = "Builder hostname; tailnet names resolve through ts-plug.";
              };
              sshUser = lib.mkOption {
                type = lib.types.str;
                description = "SSH user on the builder.";
              };
              sshKeyFile = lib.mkOption {
                type = lib.types.str;
                default = "${hostSecretsMount}/nix-builder-key";
                defaultText = lib.literalExpression ''"''${hostSecrets.mountPoint}/nix-builder-key"'';
                description = "SSH private key the root nix-daemon uses for the builder.";
              };
              publicHostKey = lib.mkOption {
                type = lib.types.str;
                description = "The builder's SSH host key, as an OpenSSH public key line.";
              };
              systems = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ pkgs.stdenv.hostPlatform.system ];
                defaultText = lib.literalExpression "[ pkgs.stdenv.hostPlatform.system ]";
                description = "Systems the builder builds for.";
              };
              maxJobs = lib.mkOption {
                type = lib.types.ints.positive;
                default = 4;
                description = "Concurrent builds on the builder.";
              };
              speedFactor = lib.mkOption {
                type = lib.types.ints.positive;
                default = 1;
                description = "Relative builder speed.";
              };
              supportedFeatures = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [
                  "nixos-test"
                  "benchmark"
                  "big-parallel"
                  "kvm"
                ];
                description = "System features the builder supports.";
              };
            };
          }
        );
      };
    };

    identity = {
      gitName = lib.mkOption {
        type = lib.types.str;
        default = name;
        defaultText = lib.literalExpression "config.modules.microvm-guest.mootWright.name";
        description = "Git author name.";
      };
      gitEmail = lib.mkOption {
        type = lib.types.str;
        default = "${name}@moot.land";
        defaultText = lib.literalExpression ''"''${name}@moot.land"'';
        description = "Git author email.";
      };
      matrixId = lib.mkOption {
        type = lib.types.str;
        default = "@${name}:moot.land";
        defaultText = lib.literalExpression ''"@''${name}:moot.land"'';
        description = "Matrix ID, exported as MOOT_MATRIX_ID.";
      };
    };

    credentials = lib.mkOption {
      type = lib.types.listOf (lib.types.strMatching "[A-Z][A-Z0-9_]*");
      default = [ ];
      description = ''
        Credentials the egress injects for this wright. Each is exported as a
        variable of the same name holding the placeholder
        TS_EGRESS_PLACEHOLDER_<NAME>; the egress swaps in the secret.
      '';
    };

    zvolSize = lib.mkOption {
      type = lib.types.ints.positive;
      description = "Size in MiB of the zvol holding the writable store.";
    };

    extraPackages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      description = "Additional system packages for the wright.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions =
      let
        inherit (cfg.network) hostAddress guestAddress prefixLength;
      in
      [
        {
          assertion = !ipv4.overlaps (ipv4.subnet guestAddress prefixLength) (ipv4.fromCidr veth.nsAddr);
          message = "mootWright reserves the namespace link ${veth.nsAddr}; pick a tap subnet outside it.";
        }
        {
          assertion = ipv4.isLink hostAddress guestAddress prefixLength;
          message = "mootWright needs hostAddress and guestAddress to be distinct host addresses of one /${toString prefixLength}.";
        }
      ];

    modules.microvm-guest = {
      autoNetSetup.enable = false;
      nixStoreBackend = "blk";
      writableStoreOverlay.zvolSize = cfg.zvolSize;
      mounts = [
        "var/lib"
        "home"
      ];
      hostSecrets = {
        enable = true;
        services = [
          "ts-plug-${name}"
          "moot-wright-credentials"
        ]
        ++ lib.optional (builder != null) "nix-daemon";
      };
    };
    microvm = {
      hypervisor = "cloud-hypervisor";
      interfaces = [
        {
          type = "tap";
          id = cfg.network.tapId;
          inherit mac;
        }
      ];
    };

    networking.useDHCP = false;
    systemd.network = {
      enable = true;
      links."30-${uplink}" = {
        matchConfig.MACAddress = mac;
        linkConfig.Name = uplink;
      };
      networks."30-${uplink}" = {
        matchConfig.MACAddress = mac;
        address = [ "${cfg.network.guestAddress}/${toString cfg.network.prefixLength}" ];
        gateway = [ cfg.network.hostAddress ];
        dns = [ cfg.network.dnsResolver ];
        networkConfig = {
          LinkLocalAddressing = "no";
          IPv6AcceptRA = false;
        };
      };
    };
    services.resolved.settings.Resolve.FallbackDNS = lib.mkForce [ cfg.network.dnsResolver ];
    environment.etc."resolv-external.conf".text = ''
      nameserver ${cfg.network.dnsResolver}
    '';

    modules.users.enable = lib.mkForce false;
    modules.services.sshd.enable = lib.mkForce false;
    security.sudo.enable = false;
    users.allowNoPasswordLogin = true;
    services.openssh = {
      startWhenNeeded = true;
      openFirewall = false;
      hostKeys = [
        {
          path = "/var/lib/ssh/ssh_host_ed25519_key";
          type = "ed25519";
        }
      ];
      settings = {
        PermitRootLogin = "prohibit-password";
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
      };
    };
    systemd.sockets.sshd.enable = false;
    systemd.services."sshd-vsock@" = {
      wants = [ "sshd-keygen.service" ];
      after = [ "sshd-keygen.service" ];
    };

    # glibc would resolve through nsncd or resolved's NSS socket, both in the
    # root namespace, so wright lookups would skip the egress DNS filter.
    # Resolve only from files and each namespace's resolv.conf.
    services.nscd.enable = false;
    system.nssModules = lib.mkForce [ ];
    system.nssDatabases.hosts = lib.mkForce [
      "files"
      "dns"
    ];

    security.pki.certificateFiles = [ egressCa ];

    programs.git = {
      enable = true;
      config = {
        user.name = cfg.identity.gitName;
        user.email = cfg.identity.gitEmail;
        init.defaultBranch = "main";
      };
    };

    environment.systemPackages =
      providers
      ++ [
        t3
        pkgs.harness-plugins-sync
      ]
      ++ cfg.extraPackages;
    environment.extraInit = ''
      if [ "$(id -u)" = ${toString uid} ]; then
      ${exports wrightEnv}
      fi
    '';

    nix = {
      settings = {
        max-jobs = cfg.nix.maxJobs;
        extra-substituters = lib.mkIf (cfg.nix.substituters != [ ]) cfg.nix.substituters;
        extra-trusted-public-keys = lib.mkIf (cfg.nix.trustedPublicKeys != [ ]) cfg.nix.trustedPublicKeys;
      };
      distributedBuilds = builder != null;
      buildMachines = lib.optional (builder != null) {
        inherit (builder)
          hostName
          sshUser
          systems
          maxJobs
          speedFactor
          supportedFeatures
          ;
        protocol = "ssh-ng";
        sshKey = builder.sshKeyFile;
      };
    };
    programs.ssh = lib.mkIf (builder != null) {
      knownHosts.${builder.hostName}.publicKey = builder.publicHostKey;
      extraConfig = ''
        Host ${builder.hostName}
          ProxyCommand ${lib.getExe' pkgs.netcat-openbsd "nc"} -X connect -x 127.0.0.1:${toString proxyPort} %h %p
      '';
    };

    users = {
      groups.${name}.gid = uid;
      groups."ts-${name}".gid = tsUid;
      users.${name} = {
        isNormalUser = true;
        inherit uid home;
        group = name;
        createHome = true;
        # T3 starts agents in scopes under this user's systemd manager.
        linger = true;
      };
      users."ts-${name}" = {
        isSystemUser = true;
        uid = tsUid;
        group = "ts-${name}";
      };
    };
    systemd.tmpfiles.rules = [ "d ${home} 0700 ${name} ${name} -" ];

    modules.networking.systemd-netns-private = {
      enable = true;
      namespaces.${netns} = veth // {
        natInterface = uplink;
      };
    };

    networking.nftables.enable = true;
    networking.nftables.tables.moot-wright = {
      family = "inet";
      content = ''
        chain input {
          type filter hook input priority filter; policy accept;
          iifname "${veth.hostIface}" counter drop
        }
        chain forward {
          type filter hook forward priority filter; policy accept;
          iifname "${veth.hostIface}" meta nfproto ipv6 counter drop
          iifname "${veth.hostIface}" ip daddr ${nonPublic} counter drop
          iifname "${veth.hostIface}" oifname "${uplink}" accept
          iifname "${veth.hostIface}" counter drop
          oifname "${veth.hostIface}" ct state { established, related } accept
          oifname "${veth.hostIface}" counter drop
        }
        chain output {
          type filter hook output priority filter; policy accept;
          meta skuid ${toString uid} counter drop
        }
        chain postrouting {
          type nat hook postrouting priority srcnat; policy accept;
          meta nfproto ipv4 ip saddr ${veth.nsAddr} oifname "${uplink}" masquerade
        }
      '';
    };

    systemd.services = {
      "systemd-netns-private-access-${netns}".serviceConfig.ExecStartPost =
        let
          inNs = "${pkgs.iproute2}/bin/ip netns exec ${netns}";
          ip = "${pkgs.iproute2}/bin/ip";
        in
        [
          "${inNs} ${lib.getExe pkgs.nftables} -f ${captureRules}"
          "-${inNs} ${ip} rule del fwmark ${captureMark} lookup ${captureTable}"
          "${inNs} ${ip} rule add fwmark ${captureMark} lookup ${captureTable}"
          "${inNs} ${ip} route replace local 0.0.0.0/0 dev lo table ${captureTable}"
        ];

      moot-wright-credentials = {
        description = "Provide the CLIProxyAPI key to ${name}";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          LoadCredential = [ "cliproxy-key:${cfg.cliproxy.keyFile}" ];
          RuntimeDirectory = "moot-wright";
          RuntimeDirectoryMode = "0755";
          ExecStart = "${lib.getExe' pkgs.coreutils "install"} -m 0400 -o ${name} -g ${name} %d/cliproxy-key ${keyFile}";
        };
      };

      moot-wright-config = {
        description = "Merge managed agent settings for ${name}";
        wantedBy = [ "multi-user.target" ];
        after = [ "systemd-tmpfiles-setup.service" ];
        unitConfig.RequiresMountsFor = [ home ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          User = name;
          Group = name;
          ExecStart = configure;
        };
      };

      "user@${toString uid}" = confinement // {
        overrideStrategy = "asDropin";
        wants = [
          credentialUnit
          configUnit
        ];
        after = confinement.after ++ [
          credentialUnit
          configUnit
        ];
        serviceConfig = inNetns;
      };

      nix-daemon = confinement // {
        environment = proxyEnv // {
          inherit (caEnv) SSL_CERT_FILE;
        };
        serviceConfig = inNetns;
      };

      "ts-plug-${name}" = confinement // {
        description = "ts-plug tailnet node for ${name}";
        wantedBy = [ "multi-user.target" ];
        wants = [ "network-online.target" ];
        after = confinement.after ++ [ "network-online.target" ];
        environment.TS_NO_LOGS_NO_SUPPORT = "true";
        serviceConfig = inNetns // {
          User = "ts-${name}";
          Group = "ts-${name}";
          StateDirectory = "ts-plug-${name}";
          StateDirectoryMode = "0700";
          LoadCredential = [ "ts-authkey:${cfg.tailscale.authKeyFile}" ];
          # ts-plug strips the key from the environment of its children.
          ExecStart = pkgs.writeShellScript "ts-plug-${name}" ''
            TS_AUTHKEY="$(< "$CREDENTIALS_DIRECTORY/ts-authkey")"
            export TS_AUTHKEY
            exec ${lib.getExe pkgs.ts-plug} ${
              lib.escapeShellArgs (
                [
                  "-hostname"
                  cfg.tailscale.hostname
                  "-dir"
                  "/var/lib/ts-plug-${name}"
                  "-https-port"
                  "443:${toString t3Port}"
                  # T3 streams over websockets and long responses.
                  "-response-header-timeout"
                  "0"
                  "-http-proxy"
                  "127.0.0.1:${toString proxyPort}"
                  "-exit-node"
                  cfg.tailscale.exitNode
                  "-dns-resolver"
                  "${cfg.network.dnsResolver}:53"
                  "-transparent-tcp"
                  transparentAddr
                  "-transparent-udp"
                  transparentAddr
                ]
                ++ lib.optionals (cfg.tailscale.advertiseTags != [ ]) [
                  "-advertise-tags"
                  (lib.concatStringsSep "," cfg.tailscale.advertiseTags)
                ]
              )
            }
          '';
          # IP_TRANSPARENT needs CAP_NET_RAW or CAP_NET_ADMIN; NET_RAW cannot
          # change the namespace's capture rules or routes. UDP replies leave
          # from the original destination, so ports such as 53 and 123 need
          # CAP_NET_BIND_SERVICE.
          AmbientCapabilities = [
            "CAP_NET_RAW"
            "CAP_NET_BIND_SERVICE"
          ];
          CapabilityBoundingSet = [
            "CAP_NET_RAW"
            "CAP_NET_BIND_SERVICE"
          ];
          Restart = "on-failure";
          RestartSec = 5;
        };
      };

      "t3-${name}" = confinement // {
        description = "T3 Code server for ${name}";
        wantedBy = [ "multi-user.target" ];
        # T3 probes for agent scopes once at startup, so the user manager
        # must already be running.
        wants = [
          "ts-plug-${name}.service"
          "user@${toString uid}.service"
        ];
        after = confinement.after ++ [
          "ts-plug-${name}.service"
          "user@${toString uid}.service"
        ];
        path = providers ++ [
          t3
          pkgs.git
          pkgs.bash
        ];
        environment = wrightEnv // {
          HOME = home;
        };
        serviceConfig = inNetns // {
          User = name;
          Group = name;
          WorkingDirectory = home;
          ExecStart = "${lib.getExe' t3 "t3"} serve --host 127.0.0.1 --port ${toString t3Port} ${home}";
          Restart = "on-failure";
          RestartSec = 5;
        };
      };
    };

    systemd.user.services.harness-plugins-sync = {
      description = "Sync agent plugin packs and update harness bindings";
      unitConfig.ConditionUser = name;
      environment = wrightEnv;
      path = [ pkgs.git ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${lib.getExe pkgs.harness-plugins-sync} sync";
      };
    };
    systemd.user.timers.harness-plugins-sync = {
      description = "Periodically sync agent plugin packs";
      wantedBy = [ "timers.target" ];
      unitConfig.ConditionUser = name;
      timerConfig = {
        OnBootSec = "1min";
        OnUnitActiveSec = "15min";
        Persistent = true;
      };
    };
  };
}
