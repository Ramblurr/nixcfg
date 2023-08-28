{
  config,
  options,
  lib,
  pkgs,
  my,
  ...
}:
with lib;
with lib.my; let
  devCfg = config.modules.dev;
  cfg = devCfg.clojure;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.dev.clojure = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      home.file."vendor/jdks/openjdk11".source = pkgs.openjdk11;
      home.file."vendor/jdks/openjdk19".source = pkgs.openjdk19;
      home.packages = with pkgs; [
        maven
        clojure
        clojure-lsp
        clj-kondo
        babashka
        polylith
      ];
      home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
        directories = [
          ".config/maven"
          ".cache/maven"
          ".config/clojure"
          ".config/clj-kondo"
          ".config/clojure-lsp"
          ".cache/clojure"
          ".cache/clojure-gitlibs"
          ".local/share/deps.clj"
        ];
      };

      xdg.configFile."clojure/deps.edn" = {
        source = ./configs/deps.edn;
      };
      xdg.configFile."clj-kondo" = {
        source = ./configs/clj-kondo;
        recursive = true;
      };
    };
  };
}
