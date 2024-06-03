{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  hn = "vpn-gateway";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../../../ramblurr.nix {
    inherit
      config
      lib
      pkgs
      inputs
      ;
  };
in
{
  imports = [
    ../../../../modules/_config/secrets.nix
    ./hardware-configuration.nix
    ./networking.nix
  ];
  system.stateVersion = "23.05";
  sops.defaultSopsFile = defaultSopsFile;
  time.timeZone = "Europe/Berlin";
  i18n.defaultLocale = "en_US.utf8";
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  environment.etc."machine-id".text = "3ce2eb0b58bf4939865529d1f3b52d95";

  boot.tmp.cleanOnBoot = true;
  zramSwap.enable = true;
  networking.hostName = hn;
  networking.domain = "";

  documentation.nixos.enable = false;
  documentation.doc.enable = false;
  modules = {
    shell = {
      atuin.enable = true;
      htop.enable = true;
      tmux.enable = true;
      zoxide.enable = true;
      zsh.enable = true;
      zsh.starship.enable = false;
    };
    services = {
      sshd.enable = true;
    };
    editors = {
      vim.enable = true;
    };
    impermanence.enable = false;
    boot.zfs = {
      enable = false;
    };
    vpn.tailscale.enable = true;
    firewall.enable = false;
    security.default.enable = true;
    networking.default.enable = false;
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
      extraGroups = [ "wheel" ];
    };
  };

  services.smartd.enable = false;
  services.vnstat.enable = true;
  services.netdata = {
    enable = true;
    config = {
      global = {
        "default port" = "19999";
        "bind to" = config.repo.secrets.local.tailscaleip;
        "history" = "604800";
        "error log" = "syslog";
        "debug log" = "syslog";
      };
    };
  };

  networking.firewall.allowedTCPPorts = [ 19999 ];
  environment.systemPackages = with pkgs; [
    tcpdump
    python311
    dosfstools
    ncdu
    rclone
    sshfs
    lshw
    iperf3
    vnstat
  ];
}
