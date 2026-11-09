{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.pi;
  jsonFormat = pkgs.formats.json { };
  instances = lib.filterAttrs (_: instance: instance.enable) cfg.instances;
in
{
  options.programs.pi = {
    enable = lib.mkEnableOption "declarative Pi agent instances";
    package = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      description = "Pi package to install; null manages configuration only.";
    };
    instances = lib.mkOption {
      default = { };
      description = "Independent Pi agent directories. Select one using PI_CODING_AGENT_DIR.";
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }: {
            options = {
              enable = lib.mkOption {
                type = lib.types.bool;
                default = true;
                description = "Whether to manage this instance.";
              };
              configDir = lib.mkOption {
                type = lib.types.str;
                default = "${config.xdg.configHome}/${name}/agent";
                description = "Agent directory, absolute or relative to the home directory.";
              };
              settings = lib.mkOption {
                inherit (jsonFormat) type;
                default = { };
                description = "Contents of settings.json. Use strings for package store paths.";
              };
              models = lib.mkOption {
                type = lib.types.nullOr jsonFormat.type;
                default = null;
                description = "Contents of models.json; null leaves it unmanaged. Use credential references, not secrets.";
              };
              fff = lib.mkOption {
                type = lib.types.nullOr jsonFormat.type;
                default = null;
                description = "Contents of pi-fff.json; null leaves it unmanaged.";
              };
            };
          }
        )
      );
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = lib.optional (cfg.package != null) cfg.package;
    home.file = lib.mkMerge (
      lib.mapAttrsToList (_: instance: {
        "${instance.configDir}/settings.json".text = builtins.toJSON instance.settings;
        "${instance.configDir}/models.json" = lib.mkIf (instance.models != null) {
          text = builtins.toJSON instance.models;
        };
        "${instance.configDir}/pi-fff.json" = lib.mkIf (instance.fff != null) {
          text = builtins.toJSON instance.fff;
        };
      }) instances
    );
  };
}
