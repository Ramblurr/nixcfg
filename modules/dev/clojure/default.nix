{
  config,
  options,
  lib,
  pkgs,
  my,
  ...
}:
with lib;
let
  devCfg = config.modules.dev;
  cfg = devCfg.clojure;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.dev.clojure = {
    enable = lib.mkEnableOption "";
  };

  config = mkIf cfg.enable {
    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
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
    };

    myhm =
      { ... }@hm:
      let
        devSDKs = with pkgs; {
          openjfx = javaPackages.openjfx21;
          gtk3 = gtk3;
          libXxf86vm = xorg.libXxf86vm;
          #jogl_2_4_0 = javaPackages.jogl_2_4_0;
        };
      in
      {
        #home.file."vendor/jdks/openjdk8".source = pkgs.jdk8;
        home.file."vendor/jdks/openjdk11".source = pkgs.jdk11;
        home.file."vendor/jdks/openjdk17".source = pkgs.jdk17;
        home.file."vendor/jdks/openjdk21".source = pkgs.jdk21;
        home.file."vendor/jdks/openjdk23".source = pkgs.jdk23;
        home.packages = with pkgs; [
          neil
          maven
          gradle
          clojure
          cljfmt
          pkgs.clojure-lsp
          clj-kondo
          leiningen
          babashka
          polylith
          javaPackages.openjfx21
          #javaPackages.jogl_2_4_0
          gtk3
          xorg.libXxf86vm
          #pkgs.my.bootleg
          jdt-language-server
        ];
        home.file.".local/dev".source =
          let
            mkEntry = name: value: {
              inherit name;
              path = value;
            };
            entries = lib.mapAttrsToList mkEntry devSDKs;
          in
          pkgs.linkFarm "local-dev" entries;
        #home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
        #  directories = [
        #    ".config/maven"
        #    ".cache/maven"
        #    ".config/clojure"
        #    ".config/clj-kondo"
        #    ".config/clojure-lsp"
        #    ".cache/clojure"
        #    ".cache/clojure-gitlibs"
        #    ".local/share/deps.clj"
        #  ];
        #};

        xdg.configFile."clojure/deps.edn" = {
          source = pkgs.replaceVars ./configs/deps.edn {
            cacheDirectory = "${hm.config.xdg.cacheHome}/.cache/clojure";
          };
        };
        xdg.configFile."clj-kondo" = {
          source = ./configs/clj-kondo;
          recursive = true;
        };
      };
  };
}
