{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../mixins/docker.nix
    ../mixins/podman.nix
    ../mixins/vscode.nix
  ];
  config = {
    environment.systemPackages = with pkgs; [
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
        ]))
    ];
    home-manager.users.ramblurr = {pkgs, ...}: {
      home.file."vendor/jdks/openjdk11".source = pkgs.openjdk11;
      home.file."vendor/jdks/openjdk19".source = pkgs.openjdk19;
      home.packages = with pkgs; [
        go
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
        docker-compose
        dig
        dogdns
        whois
        nmap
        yq
        k9s
        yamlfmt
        kustomize
        cilium-cli
        talosctl
        fluxcd
        esphome
        nodejs
        nodePackages.npm
        clojure
        clojure-lsp
        clj-kondo
        babashka
        polylith
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
          ".config/JetBrains"
          ".cache/JetBrains"
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
        ];
      };
      programs.gh = {
        enable = true;
      };
    };
  };
}
