{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ./hardware.nix
    ./immich.nix
    ./disk-config.nix
    ../../config
    ../../config/home-ops.nix
    ../../modules/site-net
  ];
  system.stateVersion = "25.11";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  sops.defaultSopsFile = ./secrets.sops.yaml;

  home-ops = {
    enable = true;
    containers.enable = true;
    # Use encrypted Tailscale transport until the data network is connected.
    zrepl = {
      enable = true;
      sourceAddress = "100.64.18.17";
      receiverAddress = "100.114.104.114";
    };
  };
  modules.boot.zfs.extraPools = lib.mkForce [ ];
  modules.vpn.tailscale.enable = true;

  modules.services.dependency-mirror = {
    enable = true;
    domain = "src.${config.repo.secrets.home-ops.workDomain}";
    githubTokenReference = "op://home-ops-prod/gickup/ol-git-archive";
  };

  modules.services.neogrok = {
    enable = true;
    listenAddress = lib.head config.site.net.prim.hosts4.peirce;
  };
  networking.firewall.interfaces.prim.allowedTCPPorts = [ config.modules.services.neogrok.port ];

  modules.services.forgejo = {
    enable = true;
    domain = "git.${config.repo.secrets.home-ops.workDomain}";
    appName = "Outskirts Labs";
    user = config.repo.secrets.home-ops.users.forgejo;
    group = config.repo.secrets.home-ops.groups.forgejo;
  };
  modules.services.caddy.defaultRequestBodyMaxSize = "100MB";
  modules.services.caddy.edge = {
    certificateHosts = [
      config.modules.services.forgejo.domain
      config.modules.services.dependency-mirror.domain
    ];
    acmeEmail = config.repo.secrets.global.email.acme;
  };

  # Use the existing untagged prim connection, matched by the physical NIC.
  systemd.network.links."10-prim" = {
    matchConfig.PermanentMACAddress = config.repo.secrets.local.lan0.hwaddr;
    linkConfig.Name = "prim";
  };
  systemd.network.networks."10-prim" = {
    networkConfig.Gateway = lib.head config.site.net.prim.hosts4.addams;
    linkConfig.RequiredForOnline = lib.mkForce "routable";
  };

  # Persistence creates the sops/age parent as root on the rolled-back root.
  # Home Manager needs to create the remaining per-user configuration here.
  systemd.tmpfiles.rules = [ "d /home/ramblurr/.config 0755 ramblurr ramblurr -" ];

  systemd.sleep.settings.Sleep = {
    AllowSuspend = false;
    AllowHibernation = false;
    AllowHybridSleep = false;
    AllowSuspendThenHibernate = false;
  };
  environment.systemPackages = [
    pkgs.gptfdisk
    pkgs.podman-compose
  ];
}
