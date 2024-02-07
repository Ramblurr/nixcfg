{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "k3s-test1";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../../../ramblurr.nix {inherit config lib pkgs inputs;};
in {
  imports = [
    ./networking.nix
  ];
  system.stateVersion = "23.11";
  sops.defaultSopsFile = defaultSopsFile;
  time.timeZone = "Europe/Berlin";
  i18n.defaultLocale = "en_US.utf8";
  sops.age.sshKeyPaths = ["/persist/etc/ssh/ssh_host_ed25519_key"];
  environment.etc."machine-id".text = "0cbc5a0908b84c809b0d02f64837ec05";

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
    };
    impermanence.enable = true;
    boot.zfs = {
      enable = true;
      zed.enable = false;
      encrypted = false;
      rootPool = "rpool";
      scrubPools = ["rpool"];
      autoSnapshot.enable = false;
      extraPools = ["tank" "tank2" "fast"];
    };
    server = {
      smtp-external-relay.enable = false;
      k3s-server = {
        enable = false;
        endpointVip = "";
        nodeIp = "";
      };
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

  services.smartd.enable = true;
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
      "/var/lib/systemd/coredump"
    ];
    files = [
    ];
  };
}
