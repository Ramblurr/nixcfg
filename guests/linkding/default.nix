{
  lib,
  inputs,
  config,
  pkgs,
  ...
}:
let
  inherit (config.users.users.linkding) uid name;
  inherit (config.repo.secrets.global) domain;
  homeDir = "/home/linkding";
  dbPassFile = config.sops.templates.db_pass_env.path;
in
{
  system.stateVersion = "24.11";
  repo.secretFiles.home-ops = ../../secrets/home-ops.nix;
  sops.defaultSopsFile = ./secrets.sops.yaml;

  microvm = {
    hypervisor = "qemu"; # required atm for sops bootstrapping
    volumes = [
      {
        # provisioned manually on host with:
        # sudo zfs create -V 2G tank/encrypted/svc/linkding-podman && sudo mkfs.ext4 /dev/zvol/tank/encrypted/svc/linkding-podman
        mountPoint = "/var/lib/podman/linkding";
        image = "/dev/zvol/tank/encrypted/svc/linkding-podman";
        autoCreate = false;
        fsType = "ext4";
      }
    ];
  };
  modules.microvm-guest = {
    host = "dewey";
    hostFQDN = "dewey.prim.${domain.home}";
    homeManager = {
      enable = true;
      username = config.repo.secrets.home-ops.users.linkding.name;
      uid = config.repo.secrets.home-ops.users.linkding.uid;
      gid = config.repo.secrets.home-ops.groups.linkding.gid;
    };
    quadlet.enable = true;
  };

  systemd.tmpfiles.rules = [
    "d /var/lib/linkding 0750 ${name} ${name}"
    "Z /var/lib/podman/linkding 0750 ${name} ${name}"
  ];

  microvm.shares =
    let
      dir = "/var/lib/linkding";
      tag = builtins.replaceStrings [ "/" ] [ "_" ] dir;
    in
    [
      {
        inherit tag;
        source = "/var/lib/linkding";
        mountPoint = dir;
        proto = "virtiofs";
      }
    ];

  sops.secrets.db_pass = { };
  sops.templates.db_pass_env = {
    owner = name;
    mode = "0400";
    content = ''
      LD_DB_PASSWORD=${config.sops.placeholder.db_pass}
    '';
  };
  networking.firewall.allowedTCPPorts = [
    8080
  ];
  home-manager.users.linkding =
    { pkgs, config, ... }:
    {
      virtualisation.quadlet.autoEscape = true;
      virtualisation.quadlet.containers.linkding = {
        autoStart = true;
        serviceConfig = {
          RestartSec = "10";
          Restart = "always";
        };
        containerConfig = {
          # renovate: docker-image
          image = "docker.io/sissbruecker/linkding:1.41.0";
          autoUpdate = "registry";
          userns = "keep-id";
          publishPorts = [ "8080:9090" ];
          environmentFiles = [ dbPassFile ];
          environments = {
            LD_AUTH_PROXY_USERNAME_HEADER = "HTTP_X_AUTHENTIK_USERNAME";
            LD_ENABLE_AUTH_PROXY = "True";
            LD_SUPERUSER_NAME = "casey";
            LD_DB_ENGINE = "postgres";
            LD_DB_HOST = "172.20.20.3";
            LD_DB_PORT = "5432";
            LD_DB_DATABASE = "linkding";
            LD_DB_USER = "linkding";
          };
          podmanArgs = [ ];
          volumes = [
            "/var/lib/linkding:/etc/linkding/data:rw"
          ];
        };
      };
    };
}
