{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "ibnsina";
  machine-id = "0cbc5a0908b84c809b0d02f64837ec05";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../../../ramblurr.nix {inherit config lib pkgs inputs;};
in {
  imports = [
    ./hardware.nix
    ./networking.nix
    ./k3s.nix
    ./disk-config.nix
  ];
  system.stateVersion = "23.11";
  sops.defaultSopsFile = defaultSopsFile;
  time.timeZone = "Europe/Berlin";
  i18n.defaultLocale = "en_US.utf8";
  sops.age.sshKeyPaths = ["/persist/etc/ssh/ssh_host_ed25519_key"];
  environment.etc."machine-id".text = machine-id;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  documentation.nixos.enable = false;
  documentation.doc.enable = false;
  modules = {
    shell = {
      atuin.enable = false;
      direnv.enable = false;
      htop.enable = true;
      tmux.enable = true;
      zoxide.enable = false;
      zsh.enable = true;
      zsh.starship.enable = false;
    };
    services = {
      sshd.enable = true;
    };
    editors = {
      vim.enable = true;
      emacs.enable = false;
    };
    impermanence.enable = true;
    boot.zfs = {
      enable = true;
      zed.enable = false;
      encrypted = true;
      rootPool = "rpool";
      scrubPools = ["rpool"];
      autoSnapshot.enable = false;
    };
    server = {
      smtp-external-relay.enable = false;
    };
    vpn.tailscale.enable = false;
    firewall.enable = true;
    security.default.enable = true;
    networking.default.enable = true;
    networking.default.hostName = hn;
    users.enable = true;
    users.primaryUser = {
      username = ramblurr.username;
      name = ramblurr.name;
      homeDirectory = ramblurr.homeDirectory;
      signingKey = ramblurr.signingKey;
      email = ramblurr.email;
      passwordSecretKey = ramblurr.passwordSecretKey;
      defaultSopsFile = defaultSopsFile;
      shell = pkgs.zsh;
      extraGroups = [
        "wheel"
      ];
    };
  };

  # Ensures that /persist/home/ramblurr exists with the correct ownership and perms before the home-manager service runs
  systemd.tmpfiles.rules = [
    "d /persist/home/${ramblurr.username} 700 ${ramblurr.username} ${ramblurr.username}"
    "d /persist/home/${ramblurr.username}/.local/state/zsh 755 ${ramblurr.username} ${ramblurr.username}"
    "d /persist/var/lib/rancher 755 root root"
    "d /persist/var/lib/cni 755 root root"
    "d /persist/etc/rancher 755 root root"
  ];

  # TODO: enable on bare metal
  services.smartd.enable = false;
  environment.systemPackages = with pkgs; [
    python312
    smartmontools
    ncdu
    lshw
    vifm
  ];
  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/var/lib/nixos"
      "/var/lib/rancher"
      "/var/lib/systemd/coredump"
      "/etc/rancher"
      "/var/lib/cni"
    ];
    files = [
    ];
  };
}
