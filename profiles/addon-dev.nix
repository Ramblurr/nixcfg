{
  config,
  lib,
  inputs,
  pkgs,
  ...
}: {
  imports = [
    ../mixins/docker.nix
    ../mixins/podman.nix
    ../mixins/vscode.nix
  ];
  environment.systemPackages = with pkgs; [
    python311Packages.netaddr
    (python311.withPackages (ps:
      with ps; [
        pip
        pytest
        virtualenv
        black
        python-lsp-black
        setuptools
        wheel
        requests
        netaddr
      ]))
  ];
  # enable cross-compilation for aarch64
  boot.binfmt.emulatedSystems = ["aarch64-linux"];
  home-manager.users.ramblurr = {pkgs, ...}: {
    home.file."vendor/jdks/openjdk11".source = pkgs.openjdk11;
    home.file."vendor/jdks/openjdk19".source = pkgs.openjdk19;
    home.packages = with pkgs; [
      roc-toolkit
      direnv
      android-tools
      heimdall-gui
      sshpass
      postgresql_15
      mariadb
      git-crypt
      ansible
      go
      git-filter-repo
      maven
      stripe-cli
      terraform
      httpie
      gettext
      jetbrains.idea-ultimate
      jetbrains.datagrip
      jetbrains.gateway
      tcpdump
      gnumake
      nix-prefetch-docker
      docker-compose
      dig
      influxdb2-cli
      dogdns
      whois
      nmap
      pre-commit
      bridge-utils
      yq
      k9s
      kubernetes-helm
      kubectl
      krew
      yamlfmt
      kustomize
      cilium-cli
      talosctl
      fluxcd
      esphome
      nodejs_20
      nodePackages_latest.pyright
      clojure
      clojure-lsp
      clj-kondo
      babashka
      polylith
      shellcheck
      cloudflared
      shadowsocks-rust
      inputs.talhelper.packages.${pkgs.stdenv.hostPlatform.system}.default
      gcc
      cmake
      libsForQt5.qt5.qttools
      libsForQt5.qt5.qtdeclarative
      subversion
      libstdcxx5
      qemu_full
    ];

    home.file.".ideavimrc" = {
      source = ../configs/ideavimrc;
    };
    xdg.configFile."ideavim" = {
      source = ../configs/ideavim;
      recursive = true;
    };
    xdg.configFile."clojure/deps.edn" = {
      source = ../configs/clojure/deps.edn;
    };
    xdg.configFile."clj-kondo" = {
      source = ../configs/clj-kondo;
      recursive = true;
    };
    xdg.configFile."npm" = {
      source = ../configs/npm;
      recursive = true;
    };
    home.persistence."/persist/home/ramblurr" = {
      directories = [
        {
          method = "symlink";
          directory = ".local/share/containers";
        }
        ".config/containers"
        ".config/JetBrains"
        ".cache/JetBrains"
        ".cache/pre-commit"
        ".local/share/JetBrains"
        ".java/.userPrefs/jetbrains"
        ".config/gh"
        ".config/github-copilot"
        ".config/maven"
        ".cache/maven"
        ".config/npm"
        ".cache/npm-packages"
        ".local/share/npm"
        ".config/clojure"
        ".config/clj-kondo"
        ".config/clojure-lsp"
        ".cache/clojure"
        ".cache/clojure-gitlibs"
        ".local/share/deps.clj"
        ".config/docker"
        ".config/k9s"
        ".config/kube"
        ".config/krew"
        ".influxdbv2"
        ".cache/pypoetry/virtualenvs/"
      ];
    };
    programs.gh = {
      enable = true;
    };
  };
}
