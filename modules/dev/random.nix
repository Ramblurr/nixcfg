{
  config,
  options,
  inputs,
  lib,
  pkgs,
  my,
  ...
}:
with lib;
with lib.my; let
  devCfg = config.modules.dev;
  cfg = devCfg.random;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.dev.random = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # enable cross-compilation for aarch64
    boot.binfmt.emulatedSystems = ["aarch64-linux"];
    myhm = {
      programs.gh = {
        enable = true;
      };
      home.packages = with pkgs; [
        nfs-utils
        inotify-tools
        roc-toolkit
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
        dig
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

      persistence = mkIf withImpermanence {
        directories = [
          {
            method = "symlink";
            directory = ".local/share/containers";
          }
          ".config/containers"
          ".cache/pre-commit"
          ".config/gh"
          ".config/github-copilot"
          ".config/docker"
        ];
      };
    };
  };
}
