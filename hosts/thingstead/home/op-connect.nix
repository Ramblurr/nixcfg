{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.programs.op-connect;
  connectTls = import ../../../configs/home-ops/onepassword-connect-tls.nix;
  op = pkgs.writeShellApplication {
    name = "op";
    text = ''
      export OP_CONNECT_HOST=${lib.escapeShellArg cfg.host}
      ${connectTls.clientExports pkgs cfg.host}
      OP_CONNECT_TOKEN="$(< ${lib.escapeShellArg cfg.tokenFile})"
      export OP_CONNECT_TOKEN
      exec ${lib.getExe pkgs._1password-cli} "$@"
    '';
  };
in
{
  options.my.programs.op-connect = {
    enable = lib.mkEnableOption "the 1Password CLI using a Connect server";
    host = lib.mkOption {
      type = lib.types.str;
      example = "https://somehost:8443";
      description = "1Password Connect server URL (OP_CONNECT_HOST).";
    };
    tokenFile = lib.mkOption {
      type = lib.types.str;
      description = "File containing the Connect access token (OP_CONNECT_TOKEN).";
    };
    package = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      default = op;
      description = "The Connect-wired `op` wrapper, for services with a fixed PATH.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ op ];
  };
}
