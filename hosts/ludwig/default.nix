{
  config,
  pkgs,
  ...
}:
let
  inherit (config.modules.users.primaryUser) username;
  site = import ./site.nix;
in
{
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
    ./users.nix
    ./zrepl.nix
    ./samba.nix
    ./nfs.nix
    ./ups.nix
    ./adguardhome.nix
    ./tailscale-watchdog.nix
    ../../config/offsite.nix
  ];
  # First NixOS install; the host previously ran TrueNAS CORE 13.
  system.stateVersion = "26.05";
  sops.defaultSopsFile = ./secrets.sops.yaml;
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  time.timeZone = "America/New_York";

  documentation.nixos.enable = false;
  documentation.doc.enable = false;

  # Remote site: never configure home.nix-lan-cache or the attic substituter
  # here. Closures are copied from the deploying host or cache.nixos.org.

  modules = {
    telemetry.smartd.enable = true;
    shell = {
      htop.enable = true;
      tmux.enable = true;
      zsh.enable = true;
      zsh.starship.enable = false;
    };
    services.sshd.enable = true;
    editors.vim.enable = true;
    impermanence.enable = true;
    boot.zfs = {
      enable = true;
      usePlymouth = false;
      # NixOS lives on the existing SSD mirror; see hardware-configuration.nix.
      inherit (site) rootPool;
      scrubPools = [
        site.rootPool
        "tank"
      ];
      autoSnapshot.enable = false;
      extraPools = [ "tank" ];
    };
    # Replaces the tailscale-ludwig jail and reuses its node state, so the
    # tailnet address, tag and approved routes carry over.
    vpn.tailscale = {
      enable = true;
      useRoutingFeatures = "server";
      exitNode = {
        enable = true;
        networkDev = site.lan.interface;
      };
    };
    firewall.enable = true;
    security.default.enable = true;
    users.enable = true;
    users.headless.enable = true;
    # The default uid belongs to a migrated account that owns data on tank.
    users.primaryUser.uid = 1010;
    users.primaryUser.extraGroups = [ "wheel" ];
  };

  services.tailscale = {
    # Only used if the reused jail state is ever rejected; see install.sh seed.
    authKeyFile = site.persist.tailscaleAuthKey;
    # The jail accepted routes; on Linux a peer's route for this LAN would
    # take over LAN traffic.
    extraSetFlags = site.tailscaleFlags ++ [ "--advertise-exit-node" ];
  };

  environment.systemPackages = [
    (pkgs.callPackage ./ludwig-boot.nix { })
    pkgs.efibootmgr
    pkgs.ipmitool
    pkgs.lm_sensors
    pkgs.ncdu
    pkgs.smartmontools
    pkgs.tcpdump
  ];

  environment.persistence."/persist" = {
    directories = [
      "/var/lib/nixos"
      "/var/lib/systemd/coredump"
    ];
    users.${username}.directories = [ ".config/sops" ];
  };
  # Keep all TrueNAS host keys: borg clients pin them in known_hosts.
  services.openssh.hostKeys = [
    {
      path = "${site.persist.sshHostKeys}/ssh_host_rsa_key";
      type = "rsa";
    }
    {
      path = "${site.persist.sshHostKeys}/ssh_host_ecdsa_key";
      type = "ecdsa";
    }
  ];

  systemd.tmpfiles.rules = [
    # The .config/sops bind mount creates ~/.config as root on the empty /home.
    "d /home/${username}/.config 0755 ${username} ${username} -"
    "d /persist/home/${username} 700 ${username} ${username}"
    "d /persist/home/${username}/.config 0775 ${username} ${username} -"
  ];
}
