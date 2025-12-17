{
  lib,
  config,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.hardware.brother-ql;
  pkg = inputs.brother_ql_web.packages.${pkgs.system}.default;
in
{

  options = {
    modules.hardware.brother-ql.enable = lib.mkEnableOption "";
  };
  config = lib.mkIf cfg.enable {
    systemd.services.brother-ql-web = {
      after = [ "network.target" ];
      description = "Brother QL Web Interface";
      wantedBy = [ "multi-user.target" ];
      environment = {
        FLASK_PRINTER = "tcp://10.9.5.2:9100";
        FLASK_MODEL = "QL-800";
        #FLASK_SERVER_PORT = "8013";
        #FLASK_LABEL_DEFAULT_SIZE = "d24";
        #FLASK_LABEL_DEFAULT_QR_SIZE = "7";
      };
      serviceConfig = {
        ExecStart = "${pkg}/bin/brother_ql_web";
        DynamicUser = true;
        SupplementaryGroups = "lp";
        Restart = "always";
      };
    };
  };
}
