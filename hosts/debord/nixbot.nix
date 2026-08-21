{
  config,
  pkgs,
  lib,
  self,
  ...
}:
# nixbot CI service (github.com/Mic92/nixbot)
#
# The service listens on a plain TCP port. Dewey Caddy terminates TLS and
# James HAProxy selects ci.<work> for James public exposure, then Dewey
# proxies the request over the prim VLAN to Debord.
let
  inherit (self.inputs.nixbot.lib) interpolate;

  nixbotPort = config.repo.secrets.home-ops.ports.nixbot;
  backupRole = "databasus_nixbot";
  databaseName = "nixbot";
  maliMgmtAddress = builtins.head config.site.net.mgmt.hosts4.mali;
  debordMgmtAddress = builtins.head config.site.net.mgmt.hosts4.debord;
  workDomain = config.repo.secrets.global.domain.work;
  localAtticSubstituter = config.repo.secrets.global.localAtticSubstituter;
  quineMgmtIp = builtins.head config.site.net.mgmt.hosts4.quine;
  atticServer = "mali";
  atticCacheName = builtins.baseNameOf localAtticSubstituter;
  atticEndpoint = lib.removeSuffix "/${atticCacheName}" localAtticSubstituter;
  atticCache = "${atticServer}:${atticCacheName}";

  atticPush = pkgs.writeShellScript "nixbot-attic-push" ''
    set -euo pipefail

    if [ "$#" -ne 1 ]; then
      echo "usage: nixbot-attic-push OUT_LINK" >&2
      exit 64
    fi

    out_link=$1
    token_file="$CREDENTIALS_DIRECTORY/attic-nixbot-token"
    attic_config_home=$(mktemp -d)
    trap 'rm -rf "$attic_config_home"' EXIT
    attic_config_dir="$attic_config_home/attic"
    attic_config="$attic_config_dir/config.toml"

    install -d -m 0700 "$attic_config_dir"
    token=$(cat "$token_file")
    {
      printf 'default-server = "%s"\n\n' ${lib.escapeShellArg atticServer}
      printf '[servers.%s]\n' ${lib.escapeShellArg atticServer}
      printf 'endpoint = "%s"\n' ${lib.escapeShellArg atticEndpoint}
      printf 'token = "%s"\n' "$token"
    } > "$attic_config"
    chmod 0600 "$attic_config"

    XDG_CONFIG_HOME="$attic_config_home" ${lib.getExe pkgs.attic-client} push --jobs 4 ${lib.escapeShellArg atticCache} "$out_link"
  '';
in
{
  assertions = [
    {
      assertion = config.modules.services.onepassword-systemd-credentials.enable;
      message = "Nixbot database backup credentials require the systemd credential provider.";
    }
    {
      assertion = !(builtins.elem 5432 config.networking.firewall.allowedTCPPorts);
      message = "Nixbot PostgreSQL must not be exposed through globally allowed TCP port 5432.";
    }
  ];

  modules.services.onepassword-systemd-credentials.consumers.databasus-nixbot-role = {
    POSTGRES_PASSWORD = "op://home-ops-prod/databasus-nixbot/password";
  };

  services.postgresql = {
    enableTCPIP = true;
    settings.listen_addresses = lib.mkForce debordMgmtAddress;
    authentication = lib.mkAfter ''
      host ${databaseName} ${backupRole} ${maliMgmtAddress}/32 scram-sha-256
      host all ${backupRole} ${maliMgmtAddress}/32 reject
    '';
  };

  services.nixbot = {
    enable = true;
    domain = "ci.${workDomain}";
    port = nixbotPort;
    # TLS is terminated by dewey's ingress; generate https:// URLs.
    useHTTPS = true;
    nginx.enable = false;
    admins = [ "github:Ramblurr" ];
    buildSystems = [ "x86_64-linux" ];
    buildConcurrency = 2;
    evalWorkerCount = 4;
    evalMaxMemorySize = 4096;
    github = {
      enable = true;
      appId = config.repo.secrets.local.nixbot.appId;
      appSecretKeyFile = config.sops.secrets."nixbot-github-app-key".path;
      webhookSecretFile = config.sops.secrets."nixbot-github-webhook-secret".path;
      oauthId = config.repo.secrets.local.nixbot.oauthId;
      oauthSecretFile = config.sops.secrets."nixbot-github-oauth-secret".path;
      # Repositories with this topic are enabled on first startup with an
      # empty database; afterwards projects are managed in the web UI.
      topic = "nixbot";
    };
    postBuildSteps = [
      {
        name = "Upload to Mali Attic";
        command = [
          "${atticPush}"
          (interpolate "%(prop:out_link)s")
        ];
        # Cache upload is part of Debord CI's success criteria: a build that
        # cannot be pushed should not be reported as a successful cached build.
        warnOnly = false;
      }
    ];
  };

  sops.secrets."nixbot-github-app-key" = {
    sopsFile = ./nixbot.sops.yaml;
  };
  sops.secrets."nixbot-github-webhook-secret" = {
    sopsFile = ./nixbot.sops.yaml;
  };
  sops.secrets."nixbot-github-oauth-secret" = {
    sopsFile = ./nixbot.sops.yaml;
  };
  sops.secrets."attic-nixbot-token" = {
    sopsFile = ./nixbot.sops.yaml;
  };

  systemd.services.nixbot.serviceConfig.LoadCredential = [
    "attic-nixbot-token:${config.sops.secrets."attic-nixbot-token".path}"
  ];

  systemd.services.databasus-nixbot-role = {
    description = "Provision the Databasus read-only role for Nixbot";
    wantedBy = [ "multi-user.target" ];
    requires = [ "postgresql.service" ];
    after = [ "postgresql.service" ];
    script = ''
      ${config.services.postgresql.package}/bin/psql \
        --no-psqlrc \
        --quiet \
        --set ON_ERROR_STOP=1 \
        --dbname postgres <<'SQL'
      \set password `cat "$CREDENTIALS_DIRECTORY/POSTGRES_PASSWORD"`
      SELECT 'CREATE ROLE ${backupRole} LOGIN'
      WHERE NOT EXISTS (SELECT FROM pg_roles WHERE rolname = '${backupRole}') \gexec
      ALTER ROLE ${backupRole}
        WITH LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS;
      SELECT format('ALTER ROLE %I PASSWORD %L', '${backupRole}', :'password') \gexec
      REVOKE ALL PRIVILEGES ON DATABASE ${databaseName} FROM ${backupRole};
      GRANT CONNECT ON DATABASE ${databaseName} TO ${backupRole};
      SQL

      ${config.services.postgresql.package}/bin/psql \
        --no-psqlrc \
        --quiet \
        --set ON_ERROR_STOP=1 \
        --dbname ${databaseName} <<'SQL'
      REVOKE ALL PRIVILEGES ON SCHEMA public FROM ${backupRole};
      GRANT USAGE ON SCHEMA public TO ${backupRole};
      REVOKE ALL PRIVILEGES ON ALL TABLES IN SCHEMA public FROM ${backupRole};
      GRANT SELECT ON ALL TABLES IN SCHEMA public TO ${backupRole};
      REVOKE ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public FROM ${backupRole};
      GRANT SELECT ON ALL SEQUENCES IN SCHEMA public TO ${backupRole};
      ALTER DEFAULT PRIVILEGES FOR ROLE nixbot IN SCHEMA public
        REVOKE ALL PRIVILEGES ON TABLES FROM ${backupRole};
      ALTER DEFAULT PRIVILEGES FOR ROLE nixbot IN SCHEMA public
        GRANT SELECT ON TABLES TO ${backupRole};
      ALTER DEFAULT PRIVILEGES FOR ROLE nixbot IN SCHEMA public
        REVOKE ALL PRIVILEGES ON SEQUENCES FROM ${backupRole};
      ALTER DEFAULT PRIVILEGES FOR ROLE nixbot IN SCHEMA public
        GRANT SELECT ON SEQUENCES TO ${backupRole};
      SQL
    '';
    serviceConfig = {
      Type = "oneshot";
      User = "postgres";
      Group = "postgres";
      RemainAfterExit = true;
      ExecStartPre = "${pkgs.coreutils}/bin/test -S /run/postgresql/.s.PGSQL.5432";
    };
  };

  # Root is impermanent; keep service state and the CI database on safe
  # datasets. The postgresql dataset is already declared in disk-config.nix;
  # declaring it here as well lets zfs-datasets create it if it is missing.
  modules.zfs.datasets.properties = {
    "rpool/encrypted/safe/svc/nixbot"."mountpoint" = "/var/lib/nixbot";
    "rpool/encrypted/safe/svc/nixbot"."com.sun:auto-snapshot" = "false";
    "rpool/encrypted/safe/svc/postgresql"."mountpoint" = "/var/lib/postgresql";
    "rpool/encrypted/safe/svc/postgresql"."com.sun:auto-snapshot" = "false";
  };

  networking.firewall.allowedTCPPorts = [ nixbotPort ];
  networking.firewall.extraInputRules = ''
    iifname "mgmt" ip saddr ${maliMgmtAddress}/32 ip daddr ${debordMgmtAddress} tcp dport 5432 accept comment "Databasus Nixbot logical backup"
  '';
  networking.hosts.${quineMgmtIp} = [ "quine" ];

  # Remote builders: quine acts as an x86_64-linux build machine.
  # maxJobs/cores are intentionally low so remote builds share quine
  # without starving its local workloads.
  nix.settings = {
    max-jobs = 2;
    cores = 4;
  };

  nix.distributedBuilds = true;
  nix.buildMachines = [
    {
      hostName = "quine";
      system = "x86_64-linux";
      protocol = "ssh-ng";
      maxJobs = 2;
      speedFactor = 2;
      supportedFeatures = [
        "nixos-test"
        "benchmark"
        "big-parallel"
        "kvm"
      ];
      sshUser = "nix-remote-build";
      sshKey = "/var/lib/nixbot/.ssh/id_ed25519";
    }
  ];

  programs.ssh.knownHosts.quine = {
    publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFICZ13T85//UvjEjf+I72FqaXyGJNt9LD4mjYSq3LTl";
  };

  # Generate the nixbot user's SSH keypair on first boot if absent.
  systemd.services.nixbot-ssh-keygen = {
    description = "Generate nixbot SSH keypair for remote builders";
    before = [ "nixbot.service" ];
    requiredBy = [ "nixbot.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = "nixbot";
      ExecStart = pkgs.writeShellScript "nixbot-ssh-keygen" ''
        if [ ! -f /var/lib/nixbot/.ssh/id_ed25519 ]; then
          mkdir -p /var/lib/nixbot/.ssh
          chmod 700 /var/lib/nixbot/.ssh
          ${lib.getExe' pkgs.openssh "ssh-keygen"} -t ed25519 \
            -f /var/lib/nixbot/.ssh/id_ed25519 -N "" -C "nixbot@debord"
        fi
      '';
    };
  };
}
