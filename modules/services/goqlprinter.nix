{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.goqlprinter;
  json = pkgs.formats.json { };
  bundledSvgFontDirs = [
    "${pkgs.dejavu_fonts}/share/fonts"
    "${pkgs.goqlprinter}/share/goqlprinter/fonts"
  ];
in
{
  options.modules.services.goqlprinter = {
    enable = lib.mkEnableOption "the goqlprinter web UI and API";

    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "Address on which to serve the web UI and API.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8000;
      description = "TCP port on which to serve the web UI and API.";
    };

    networkPrinter = lib.mkOption {
      type = lib.types.nullOr (
        lib.types.submodule {
          options = {
            address = lib.mkOption {
              type = lib.types.str;
              description = "Raw print endpoint as host:port.";
            };
            model = lib.mkOption {
              type = lib.types.str;
              description = "Brother QL model used to encode print jobs.";
            };
          };
        }
      );
      default = null;
      description = "A configured network printer. Otherwise, use native printer discovery.";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.etc."labelprinter/config.json".source = json.generate "goqlprinter.json" {
      server = {
        host = cfg.listenAddress;
        inherit (cfg) port;
      };
      app = {
        font_dirs = [
          "${pkgs.goqlprinter}/share/goqlprinter/fonts"
          "${pkgs.dejavu_fonts}/share/fonts"
        ];
        svg_font_dirs = [ pkgs.label-fonts.fontsDir ] ++ bundledSvgFontDirs;
        font_catalog = pkgs.label-fonts.mkCatalog bundledSvgFontDirs;
      }
      // lib.optionalAttrs (cfg.networkPrinter != null) {
        backend = "tcp";
        tcp_address = cfg.networkPrinter.address;
        default_printer = cfg.networkPrinter.model;
      };
    };

    systemd.services.goqlprinter = {
      description = "Brother QL label printing web UI";
      restartTriggers = [ config.environment.etc."labelprinter/config.json".source ];
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        ExecStart = "${lib.getExe pkgs.goqlprinter} serve";
        DynamicUser = true;
        StateDirectory = "goqlprinter";
        WorkingDirectory = "/var/lib/goqlprinter";
        Restart = "on-failure";
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        MemoryMax = "1G";
      }
      // lib.optionalAttrs (cfg.networkPrinter == null) {
        SupplementaryGroups = [ "lp" ];
      };
    };
  };
}
