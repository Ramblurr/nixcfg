{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  devCfg = config.modules.dev;
  cfg = devCfg.clojure;
  inherit (config.modules.users.primaryUser) username;
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
      hm:
      let
        devSDKs = with pkgs; {
          #openjfx = javaPackages.openjfx24;
          #gtk3 = gtk3;
          #libXxf86vm = xorg.libXxf86vm;
          #jogl_2_4_0 = javaPackages.jogl_2_4_0;
        };
      in
      {
        home.file."vendor/jdks/openjdk21".source = pkgs.jdk21;
        home.file."vendor/jdks/openjdk25".source = pkgs.jdk25;
        home.file."vendor/jdks/graalvm-ce".source = pkgs.graalvmPackages.graalvm-ce;
        home.file."vendor/jdks/graalvm-oracle".source = pkgs.graalvmPackages.graalvm-oracle;
        home.packages = with pkgs; [
          jdk25
          neil
          maven
          gradle
          brepl
          cljfmt
          clojure
          clojure-lsp
          clj-kondo
          leiningen
          babashka
          bbin
          polylith
          gtk3
          xorg.libXxf86vm
          jdt-language-server
          (pkgs.writeScriptBin "run-clojure-mcp" ''
            #!/usr/bin/env bash
            if [ ! -f "deps.edn" ]; then
              echo "Error: deps.edn not found in current directory" >&2
              exit 1
            fi
            exec > >(tee -a clojure-mcp-stdout.log)
            exec 2>> clojure-mcp-stdout.log
            set -euo pipefail
            PORT_FILE=''${1:-.nrepl-port}
            PORT=4888
            if [ -f "$PORT_FILE" ]; then
              PORT=$(cat ''${PORT_FILE})
            fi
            set -a
            source ~/${hm.config.sops.secrets.llm-keys.path}
            set +a
            unset ANTHROPIC_API_KEY
            ${clojure}/bin/clojure -X:mcp/clojure :port $PORT
          '')
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
            cacheDirectory = hm.config.xdg.cacheHome;
          };
        };

        xdg.configFile."nrepl/nrepl.edn" = {
          text = ''{:dynamic-vars {clojure.core/*warn-on-reflection* true}}'';
        };
        xdg.configFile."clj-kondo" = {
          source = ./configs/clj-kondo;
          recursive = true;
        };
        home.sessionVariables = {
          SDPX_LICENSES_PATH = "${pkgs.spdx-license-list-data.json}/json";
        };
      };
  };
}
