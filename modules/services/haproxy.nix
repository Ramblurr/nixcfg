{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.server.haproxy;

  sniBackends = map (backend: ''
    backend ${backend.upstreamName}
          mode tcp
          server ${backend.upstreamName} ${backend.upstreamHost}:${toString backend.upstreamPort}
  '') cfg.sniBackends;

  sniBackendsStr = concatStringsSep "\n  " sniBackends;

  sniDomainsPerBackend = map (
    backend:
    map (domain: ''
      use_backend ${backend.upstreamName} if { req.ssl_sni -i ${domain} }
    '') backend.sniDomains
  ) cfg.sniBackends;
  sniDomainsPerBackendStr = concatMapStringsSep "\n  " (
    x: concatStringsSep "\n  " x
  ) sniDomainsPerBackend;
in
{

  options.modules.server.haproxy = {
    enable = mkEnableOption "haproxy service";
    defaultBackend = mkOption { type = types.str; };
    sniBackends = mkOption {
      type = types.listOf (
        types.submodule {
          options = {
            upstreamName = mkOption { type = types.str; };
            upstreamHost = mkOption { type = types.str; };
            upstreamPort = mkOption { type = types.int; };
            sniDomains = mkOption { type = types.listOf types.str; };
          };
        }
      );
    };
  };
  config = mkIf cfg.enable {
    services.haproxy = {
      enable = true;
      config = ''
        global
          maxconn 4096
          user haproxy
          group haproxy
          daemon
          log /dev/log  local0 debug

        defaults
          log global
          mode tcp
          option tcplog
          option dontlognull
          timeout connect 10s
          timeout client 1m
          timeout server 1m

        frontend tls_sni_frontend
          bind *:443
          tcp-request inspect-delay 5s
          tcp-request content accept if { req.ssl_hello_type 1 }
          log-format "%ci:%cp [%t] %ft %b/%s %Tw/%Tc/%Tt %B %ts %ac/%fc/%bc/%sc/%rc %sq/%bq SNI:%[ssl_fc_sni]"
          log global
          default_backend ${cfg.defaultBackend}

          ${sniDomainsPerBackendStr}
          ${sniBackendsStr}

      '';
    };
  };
}
