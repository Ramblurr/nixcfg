{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.services.github-runner;
in
{
  options.modules.services.github-runner = {
    enable = lib.mkEnableOption "";
    runnerName = lib.mkOption {
      type = lib.types.uniq lib.types.str;
      default = "my-runner";
    };
    url = lib.mkOption {
      type = lib.types.uniq lib.types.str;
      default = "";
    };
    extraLabels = mkOption {
      type = types.listOf types.str;
      default = [ ];
    };
  };
  config = mkIf cfg.enable {
    sops.secrets."github-runner.token" = { };
    services.github-runners."${cfg.runnerName}" = {
      enable = true;
      replace = true;
      name = cfg.runnerName;
      nodeRuntimes = [ "node20" ];
      extraPackages = with pkgs; [
        gh
        docker
        gawk
        nix
      ];
      inherit (cfg) url;
      tokenFile = "/run/secrets/github-runner.token";
      inherit (cfg) extraLabels;
      serviceOverrides = {
        Group = "docker";
      };
    };
    #environment.persistence."/persist" = {
    #  directories = [
    #  ];
    #};
  };
}
