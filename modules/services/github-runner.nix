{ options, config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.services.github-runner;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.services.github-runner = {
    enable = mkBoolOpt false;
    runnerName = mkStrOpt "my-runner";
    url = mkStrOpt "";
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
      extraPackages = with pkgs; [ gh docker gawk nix ];
      url = cfg.url;
      tokenFile = "/run/secrets/github-runner.token";
      extraLabels = cfg.extraLabels;
      serviceOverrides = { Group = "docker"; };
    };
    #environment.persistence."/persist" = {
    #  directories = [
    #  ];
    #};
  };
}
