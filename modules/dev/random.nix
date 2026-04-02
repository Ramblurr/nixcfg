{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
with lib;
let
  devCfg = config.modules.dev;
  cfg = devCfg.random;
  inherit (config.modules.users.primaryUser) username;
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
      home.sessionVariables = {
        KUBECONFIG = "$XDG_CONFIG_HOME/kube/config";
        KREW_ROOT = "$XDG_CONFIG_HOME/krew";
        KUBE_CONFIG_PATH = "$KUBECONFIG";
        PSQL_HISTORY = "$XDG_CACHE_HOME/psql_history";
        DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
        FLY_CONFIG_DIR = "$XDG_CONFIG_HOME/fly";
      };
      programs.gh.enable = true;
      home.packages = with pkgs; [
        mani
        ghq
        sprite-bin
        inputs.spdx-util.packages.${pkgs.stdenv.hostPlatform.system}.default
        tailwindcss_4
        hclfmt
        virt-manager
        cue
        #semgrep
        #rpi-imager # 2025-10-23 broken build
        nfs-utils
        inotify-tools
        roc-toolkit
        gcc
        direnv
        android-tools
        socat
        flyctl
        sshpass
        #gitbutler-bin
        steam-run
        postgresql_15
        mariadb
        ansible
        go
        git-filter-repo
        stripe-cli
        terraform
        opentofu
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
        doggo
        scc
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
        bfg-repo-cleaner
      ];
    };
  };
}
