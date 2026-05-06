{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.hosts.james.ingress;
  routes = import ./ingress-routes.nix { inherit config; };

  splitServices = domains: {
    exact = builtins.filter (domain: !(lib.hasPrefix "." domain)) domains;
    suffix = builtins.filter (domain: lib.hasPrefix "." domain) domains;
  };

  mkBackendRules =
    backendName: domains:
    let
      serviceSets = splitServices domains;
      exactRules = map (
        domain: "use_backend ${backendName} if { req.ssl_sni -i ${domain} }"
      ) serviceSets.exact;
      suffixRules = map (
        domain: "use_backend ${backendName} if { req.ssl_sni -m end ${domain} }"
      ) serviceSets.suffix;
    in
    exactRules ++ suffixRules;

  frontendRules = lib.concatStringsSep "\n          " (
    (mkBackendRules "bk_dewey" routes.deweyServices)
    ++ (mkBackendRules "bk_thingstead" routes.thingsteadServices)
    ++ (mkBackendRules "bk_james_local" routes.localServices)
  );
in
{
  config = lib.mkIf (cfg.implementation == "haproxy") {
    networking.firewall.allowedTCPPorts = [ 443 ];

    users.users.haproxy.extraGroups = [ config.services.nginx.group ];

    services.haproxy = {
      enable = true;
      package = pkgs.haproxy;
      config = ''
        global
          maxconn 4096
          user haproxy
          group haproxy
          daemon
          log /dev/log local0 info

        defaults
          log global
          mode tcp
          option tcplog
          option dontlognull
          timeout connect 10s
          timeout client 1m
          timeout server 1m

        resolvers tailscale
          parse-resolv-conf
          resolve_retries 3
          timeout retry 1s
          hold valid 10s

        frontend tls_sni_frontend
          bind *:443
          tcp-request inspect-delay 5s
          tcp-request content accept if { req.ssl_hello_type 1 }
          default_backend bk_james_local
          ${frontendRules}

        backend bk_dewey
          mode tcp
          server dewey ${routes.haproxyBackends.dewey.host}:${toString routes.haproxyBackends.dewey.port} resolvers tailscale init-addr libc,none

        backend bk_thingstead
          mode tcp
          server thingstead ${routes.haproxyBackends.thingstead.host}:${toString routes.haproxyBackends.thingstead.port} resolvers tailscale init-addr libc,none

        backend bk_james_local
          mode tcp
          server james-local /run/nginx/james-ingress.sock send-proxy
      '';
    };

    systemd.services.haproxy.serviceConfig = {
      AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
    };
  };
}
