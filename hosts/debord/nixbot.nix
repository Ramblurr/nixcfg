{
  config,
  pkgs,
  lib,
  self,
  ...
}:
# nixbot CI service (github.com/Mic92/nixbot)
#
# The service listens on a plain TCP port; TLS termination and external
# exposure happen on dewey's nginx ingress (ci.<work> -> debord:<port>),
# which in turn is reachable from the internet via the james gost tunnel.
let
  inherit (self.inputs.nixbot.lib) interpolate;

  nixbotPort = config.repo.secrets.home-ops.ports.nixbot;
  workDomain = config.repo.secrets.global.domain.work;
  homeDomain = config.repo.secrets.global.domain.home;
  localAtticSubstituter = config.repo.secrets.global.localAtticSubstituter;
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
  services.nixbot = {
    enable = true;
    domain = "ci.${workDomain}";
    port = nixbotPort;
    # TLS is terminated by dewey's ingress; generate https:// URLs.
    useHTTPS = true;
    nginx.enable = false;
    admins = [ "github:ramblurr" ];
    buildSystems = [ "x86_64-linux" ];
    evalWorkerCount = 4;
    evalMaxMemorySize = 4096;
    github = {
      enable = true;
      appId = config.repo.secrets.local.nixbot.appId;
      appSecretKeyFile = config.sops.secrets."nixbot-github-app-key".path;
      webhookSecretFile = config.sops.secrets."nixbot-github-webhook-secret".path;
      oauthId = config.repo.secrets.local.nixbot.oauthId;
      oauthSecretFile = config.sops.secrets."nixbot-github-oauth-secret".path;
      # Required to see private repo builds in the UI; grants the GitHub
      # "repo" OAuth scope (read+write) stored server-side for the session.
      oauthPrivateRepoScope = true;
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

  # Remote builders: quine acts as an x86_64-linux build machine.
  # maxJobs/cores are intentionally low so remote builds share quine
  # without starving its local workloads.
  nix.distributedBuilds = true;
  nix.buildMachines = [
    {
      hostName = "quine.prim.${homeDomain}";
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

  programs.ssh.knownHosts."quine.prim.${homeDomain}" = {
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
