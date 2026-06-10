{
  config,
  ...
}:
# nixbot CI service (github.com/Mic92/nixbot)
#
# The service listens on a plain TCP port; TLS termination and external
# exposure happen on dewey's nginx ingress (ci.<work> -> debord:<port>),
# which in turn is reachable from the internet via the james gost tunnel.
let
  nixbotPort = config.repo.secrets.home-ops.ports.nixbot;
  workDomain = config.repo.secrets.global.domain.work;
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
      # Repositories with this topic are enabled on first startup with an
      # empty database; afterwards projects are managed in the web UI.
      topic = "nixbot";
    };
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

  # Root is impermanent; keep service state and the CI database on safe
  # datasets. The postgresql dataset is already declared in disk-config.nix;
  # declaring it here as well lets zfs-datasets create it if it is missing.
  modules.zfs.datasets.properties = {
    "rpool/encrypted/safe/svc/nixbot"."mountpoint" = "/var/lib/nixbot";
    "rpool/encrypted/safe/svc/nixbot"."com.sun:auto-snapshot" = "false";
    "rpool/encrypted/safe/svc/postgresql"."mountpoint" = "/var/lib/postgresql";
    "rpool/encrypted/safe/svc/postgresql"."com.sun:auto-snapshot" = "false";
  };

  # Reached only by dewey's nginx over the prim VLAN.
  networking.firewall.allowedTCPPorts = [ nixbotPort ];
}
