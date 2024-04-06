{
  options,
  config,
  lib,
  pkgs,
  inputs,
  mine,
  ...
}:
let
  cfg = config.modules.services.invoiceninja;
  home-ops = config.repo.secrets.home-ops;
in
{
  options.modules.services.invoiceninja = {
    enable = lib.mkEnableOption "invoiceninja";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "invoices.example.com";
      description = "The domain to use for the invoiceninja";
    };

    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
  };
  imports = [ "${inputs.nixpkgs-mine}/nixos/modules/services/web-apps/invoiceninja.nix" ];
  config = lib.mkIf cfg.enable {
    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/invoiceninja"."mountpoint" = config.services.invoiceninja.dataDir;
      "rpool/encrypted/safe/svc/invoiceninja"."com.sun:auto-snapshot" = "false";
    };
    sops.secrets."invoiceninja/env" = {
      owner = config.services.invoiceninja.user;
      group = config.services.invoiceninja.group;
      mode = "400";
    };
    services.invoiceninja = {
      enable = true;
      domain = cfg.domain;
      package = mine.invoiceninja;
      environment = {
        LOG_CHANNEL = "errorlog";
        LOG_LEVEL = "debug";
      };
      environmentFile = config.sops.secrets."invoiceninja/env".path;
      nginx = {
        useACMEHost = cfg.ingress.domain;
        onlySSL = true;
        kTLS = true;
      };
    };
  };
}
