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
in
{
  system.stateVersion = "24.11";
  repo.secretFiles.home-ops = ../../secrets/home-ops.nix;
  modules.microvm-guest = {
    host = "dewey";
    hostFQDN = "bookmarks.${domain}";
    homeManager = {
      enable = true;
      username = "linkding";
      uid = config.repo.secrets.home-ops.users.linkding.uid;
      gid = config.repo.secrets.home-ops.groups.linkding.gid;
    };
    quadlet.enable = true;
  };

  systemd.tmpfiles.rules = [
    "d /var/lib/linkding 0750 ${name} ${name}"
  ];

  microvm.hypervisor = "qemu";
  microvm.credentialFiles = {
    "SOPS_AGE_KEY" = "/run/secrets/linkding_sops_age_key";
  };
  systemd.services.sshd = {
    serviceConfig = {
      ImportCredential = "SOPS_AGE_KEY";
    };
    preStart = ''
      # Make sure we don't write to stdout, since in case of
      # socket activation, it goes to the remote side (#19589).
      exec >&2
      mkdir -p /etc/ssh
      cat $CREDENTIALS_DIRECTORY/SOPS_AGE_KEY > /etc/ssh/ssh_host_ed25519_key
      chmod 0600 /etc/ssh/ssh_host_ed25519_key
    '';
  };
  ##microvm.qemu.machine = "q35";
  #microvm.qemu.extraArgs = [
  #  # only works with  microvm.qemu.machine = "q35";
  #  #"-smbios" "type=11,value=io.systemd.credential:mycred=supersecret"

  #  #WORKS
  #  "-fw_cfg"
  #  "name=opt/io.systemd.credentials/mycred,string=supersecret"
  #];
  #microvm.cloud-hypervisor.platformOEMStrings = [
  #  "io.systemd.credential:APIKEY=supersecret"
  #];
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

  home-manager.users.linkding =
    { pkgs, config, ... }:
    {
      #virtualisation.quadlet.containers.linkding = {
      #  autoStart = false;
      #  serviceConfig = {
      #    RestartSec = "10";
      #    Restart = "always";
      #  };
      #  containerConfig = {
      #    # renovate: docker-image
      #    image = "docker.io/sissbruecker/linkding:1.38.1";
      #    autoUpdate = "registry";
      #    userns = "keep-id";
      #    publishPorts = [ "8080:9090" ];
      #    environments = {
      #      LD_AUTH_PROXY_USERNAME_HEADER = "HTTP_X_AUTHENTIK_USERNAME";
      #      LD_ENABLE_AUTH_PROXY = "True";
      #      LD_SUPERUSER_NAME = "casey";
      #      LD_DB_ENGINE = "postgres";
      #      LD_DB_HOST = "/run/postgresql";
      #      LD_DB_PORT = "";
      #      LD_DB_DATABASE = "linkding";
      #      LD_DB_USER = "linkding";
      #      LD_DB_PASSWORD = "";
      #    };
      #    podmanArgs = [ ];
      #    volumes = [
      #      "/var/lib/linkding:/etc/linkding/data:rw"
      #      "/run/postgresql:/run/postgresql:ro"
      #    ];
      #  };
      #};
    };
}
