{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.services.echo-server;

  httpPort = toString cfg.ports.http;
in
{
  options.modules.services.echo-server = {
    enable = lib.mkEnableOption "echo-server";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "echo-test.example.com";
      description = "The domain to use for the echo server";
    };
    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        default = 9992;
        description = "The HTTP port to use for the echo server";
      };
    };

    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
  };
  config = lib.mkIf cfg.enable {
    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
    virtualisation.oci-containers.containers.echo-server = {
      # renovate: datasource=docker depName=mendhak/http-https-echo
      image = "docker.io/mendhak/http-https-echo:31";
      autoStart = true;
      ports = [ "127.0.0.1:${httpPort}:8080" ];
    };
    services.nginx.virtualHosts.${cfg.domain} = {
      useACMEHost = cfg.ingress.domain;
      onlySSL = true;
      kTLS = true;
      extraConfig = ''
        client_max_body_size 0;
        client_header_buffer_size 64k;
      '';
      locations."/" = {
        proxyPass = "http://127.0.0.1:${httpPort}";
        recommendedProxySettings = true;
      };
    };
  };
}
