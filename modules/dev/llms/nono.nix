{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.dev.llms;
  jsonFormat = pkgs.formats.json { };
  llmAgents = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
  nono-eca = pkgs.writeShellScriptBin "nono-eca" ''
    set -euo pipefail
    existing_config="''${ECA_CONFIG:-}"
    if [ -z "$existing_config" ]; then existing_config='{}'; fi
    ECA_CONFIG=$(printf '%s' "$existing_config" | ${lib.getExe pkgs.jq} \
      --argjson routing ${lib.escapeShellArg (builtins.toJSON cfg.nono.ecaSettings)} '. * $routing')
    export ECA_CONFIG
    ${lib.optionalString (cfg.onepasswordConnect.tokenFile != null) ''
      export OP_CONNECT_HOST=${lib.escapeShellArg cfg.onepasswordConnect.host}
      OP_CONNECT_TOKEN="$(< ${lib.escapeShellArg cfg.onepasswordConnect.tokenFile})"
      export OP_CONNECT_TOKEN
    ''}
    exec ${lib.getExe llmAgents.nono} --silent run \
      --profile eca \
      --allow-cwd \
      -- ${lib.getExe llmAgents.eca} "$@"
  '';
in
{
  options.modules.dev.llms.nono = {
    profiles = lib.mkOption {
      type = lib.types.attrsOf lib.types.attrs;
      default = { };
      description = "Host-declared nono profiles, written as JSON.";
    };
    ecaSettings = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Host-declared ECA settings merged into ECA_CONFIG by nono-eca.";
    };
  };
  config = lib.mkIf (cfg.enable && cfg.nono.enable) {
    myhm = {
      xdg.configFile = lib.mapAttrs' (
        name: profile:
        lib.nameValuePair "nono/profiles/${name}.json" {
          source = jsonFormat.generate "nono-profile-${name}.json" profile;
        }
      ) cfg.nono.profiles;
      home.packages = [ llmAgents.nono ] ++ lib.optional (cfg.nono.profiles ? eca) nono-eca;
    };
  };
}
