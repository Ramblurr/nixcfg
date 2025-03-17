{
  config,
  options,
  inputs,
  mine,
  lib,
  pkgs,
  ...
}:
with lib;
let
  devCfg = config.modules.dev;
  cfg = devCfg.random;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.dev.random = {
    enable = lib.mkEnableOption "";
  };

  config = mkIf cfg.enable {
    # enable cross-compilation
    boot.binfmt.emulatedSystems = [
      "aarch64-linux"
      "armv6l-linux"
      "armv7l-linux"
    ];
    virtualisation.spiceUSBRedirection.enable = true;
    environment.systemPackages = with pkgs; [
      spice-gtk
      quickemu
      #quickgui
    ];

    programs.nix-ld = {
      enable = true;
      libraries = with pkgs; [
        stdenv.cc.cc
        openssl
        glib
      ];
    };

    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".local/share/containers"
          ".config/containers"
          ".cache/pre-commit"
          ".config/gh"
          ".config/github-copilot"
          ".config/docker"
        ];
      };
    };
    myhm = {
      programs.gh = {
        enable = true;
      };
      home.packages = with pkgs; [
        mine.claude-code
        hclfmt
        virt-manager
        cue
        rpi-imager
        nfs-utils
        inotify-tools
        roc-toolkit
        gcc
        direnv
        android-tools
        sshpass
        postgresql_15
        mariadb
        ansible
        go
        git-filter-repo
        stripe-cli
        terraform
        httpie
        gettext
        tcpdump
        gnumake
        nix-prefetch-docker
        docker-compose
        hashcat
        hashcat-utils
        dig
        gitleaks
        dogdns
        whois
        nmap
        pre-commit
        bridge-utils
        yq
        yamlfmt
        yamllint
        esphome
        shellcheck
        shadowsocks-rust
        ssh-to-age
      ];
    };
  };
}
