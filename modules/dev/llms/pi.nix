{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.dev.llms;
  instructions = lib.concatStringsSep "\n\n" (
    lib.filter (s: s != "") [
      cfg.agents.preamble
      cfg.pi.agents.preamble
      cfg.agents.extraText
      cfg.pi.agents.extraText
    ]
  );
in
{
  options.modules.dev.llms.pi = {
    package = lib.mkOption {
      type = lib.types.package;
      default = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi;
      description = "Pi CLI package.";
    };
    instances = lib.mkOption {
      type = lib.types.attrsOf lib.types.attrs;
      default = { };
      description = "Host-declared Pi instances, using programs.pi.instances fields.";
    };
  };
  config = lib.mkIf (cfg.enable && cfg.pi.enable) {
    myhm = {
      imports = [ ./pi-nix.nix ];
      home.packages = lib.optional (
        cfg.onepasswordConnect.tokenFile != null
      ) cfg.onepasswordConnect.package;
      programs.pi = {
        enable = true;
        package = cfg.pi.package;
        instances = lib.mapAttrs (
          _: instance:
          instance
          // {
            instructions = lib.optionalString (cfg.agents.enable && cfg.pi.agents.enable) (
              instance.instructions or instructions
            );
            settings = lib.recursiveUpdate cfg.pi.settings (instance.settings or { });
            models =
              if (instance.models or null) == null && cfg.pi.models == { } then
                null
              else
                lib.recursiveUpdate cfg.pi.models (
                  if (instance.models or null) == null then { } else instance.models
                );
          }
        ) cfg.pi.instances;
      };
    };
  };
}
