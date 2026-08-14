{
  config,
  lib,
  ...
}:
let
  inherit (config.networking) hostName;
  inherit (config.modules.users.primaryUser) username;
  homeDomain = config.repo.secrets.global.domain.home;
  pocketIdDomain = "id.${homeDomain}";
in
{
  imports = [
    ./disk-config.nix
    ./hardware.nix
    ./users.nix
    ../../config/offsite.nix
    ../../config/hetzner-cloud-ccx.nix
    ./ingress.nix
    ./ingress-haproxy.nix
    ./web.nix
    ./web/hook.nix
    ./web/work.nix
    ./web/work-docs.nix
    ./web/personal.nix
    ./web/personal-site.nix
    ./web/personals.nix
    ./web/partner.nix
    ./goaccess.nix
    ./goatcounter.nix
    ./crowdsec.nix
  ];
  system.stateVersion = "24.11";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  sops.defaultSopsFile = ./secrets.sops.yaml;
  sops.secrets = {
    home-oauth2-proxy-client-secret = { };
    home-oauth2-proxy-cookie-secret = { };
  };
  sops.templates.home-oauth2-proxy-env = {
    owner = "oauth2-proxy";
    group = "oauth2-proxy";
    mode = "0400";
    restartUnits = [ "oauth2-proxy.service" ];
    content = ''
      OAUTH2_PROXY_CLIENT_SECRET=${config.sops.placeholder.home-oauth2-proxy-client-secret}
      OAUTH2_PROXY_COOKIE_SECRET=${config.sops.placeholder.home-oauth2-proxy-cookie-secret}
    '';
  };
  time.timeZone = "Europe/Berlin";

  networking.hostId = lib.my.generateHostId hostName;
  networking.hosts."127.0.0.1" = [ pocketIdDomain ];
  networking.firewall.enable = false;

  modules = {
    shell = {
      htop.enable = true;
      tmux.enable = true;
      zsh.enable = true;
    };
    services = {
      pocket-id.enable = true;
      oauth2-proxy = {
        enable = true;
        issuerURL = "https://${pocketIdDomain}";
        publicHost = pocketIdDomain;
        cookieDomain = ".${homeDomain}";
        clientID = config.repo.secrets.local.oauth2ProxyClientId;
        secretEnvironmentFile = config.sops.templates.home-oauth2-proxy-env.path;
      };
      sshd.enable = true;
    };
    editors = {
      vim.enable = true;
    };
    impermanence.enable = true;
    vpn.tailscale = {
      enable = true;
      exitNode = {
        enable = true;
        networkDev = "enp1s0";
      };
    };
    boot.zfs = {
      enable = false;
      encrypted = false;
      rootPool = "rpool";
      scrubPools = [ "rpool" ];
      extraPools = [ ];
      autoSnapshot.enable = false;
    };
    zfs.datasets.enable = true;
    security.default.enable = true;
    users.enable = true;
    users.primaryUser.extraGroups = [
      "wheel"
    ];
  };

  hosts.james.ingress.implementation = "haproxy";

  security.acme.certs.${pocketIdDomain}.domain = pocketIdDomain;
  services.nginx.virtualHosts.${pocketIdDomain} = {
    useACMEHost = pocketIdDomain;
    forceSSL = true;
    kTLS = true;
    http3 = false;
    quic = false;
    locations."/" = {
      proxyPass = "http://127.0.0.1:1411";
      recommendedProxySettings = true;
      proxyWebsockets = true;
    };
  };

  systemd.services.oauth2-proxy = {
    requires = [ "sops-install-secrets.service" ];
    after = [ "sops-install-secrets.service" ];
  };

  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/var/lib/nixos"
      "/var/lib/systemd/coredump"
    ];
    files = [ ];
    users.${username}.directories = [ ".config/sops" ];
  };

  systemd.tmpfiles.rules = [
    "d /persist/home/${username} 700 ${username} ${username}"
    "d /persist/home/${username}/.config 0775 ${username} ${username}  -"
    "d /persist/home/${username}/.local 755 ${username} ${username}"
    "d /persist/home/${username}/.local/state 755 ${username} ${username}"
    "d /persist/home/${username}/.local/state/zsh 755 ${username} ${username}"
  ];

}
