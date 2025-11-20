{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.desktop.mako;
  username = config.modules.users.primaryUser.username;
in
{
  options.modules.desktop.mako = {
    enable = lib.mkEnableOption "Enable mako";
  };
  config = lib.mkIf cfg.enable {
    myhm =
      { pkgs, ... }@hm:
      {
        services.mako = {
          enable = true;
          settings = {
            font = "monospace 18";
            anchor = "top-center";
            markup = true;
            padding = "5";
            background-color = "#282828";
            text-color = "#ebdbb2";
            progress-color = "#ebdbb2";
            border-color = "#928374";
            border-size = 3;
            border-radius = 6;
            width = 600;
          };
        };
      };
  };
}
