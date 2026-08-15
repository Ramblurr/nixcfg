{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.services.ingress;
  directWanVirtualHosts = lib.filterAttrs (_: service: service.directWan) cfg.virtualHosts;
  hasDirectWanVirtualHosts = directWanVirtualHosts != { };
  caddyEdgeEnabled = lib.attrByPath [
    "modules"
    "services"
    "caddy-security"
    "edge"
    "enable"
  ] false config;
  caddyCertificateDomains =
    lib.attrByPath
      [
        "modules"
        "services"
        "caddy-security"
        "edge"
        "certificateDomains"
      ]
      [ ]
      config;
  caddyCertificateHosts =
    lib.attrByPath
      [
        "modules"
        "services"
        "caddy-security"
        "edge"
        "certificateHosts"
      ]
      [ ]
      config;
  caddyManagesCertificates =
    caddyEdgeEnabled && (caddyCertificateDomains != [ ] || caddyCertificateHosts != [ ]);

  mkVirtualHost =
    name: service: directWan:
    let
      hasUpstream = service.upstream != null;
    in
    {
      useACMEHost = service.acmeHost;
      forceSSL = !directWan;
      onlySSL = directWan;
      kTLS = true;
      inherit (service) extraConfig;
      http3 = !directWan && service.http3.enable;
      http2 = false;
      quic = !directWan && service.http3.enable;
      inherit (service) root;
      locations = {
        "/" = {
          proxyPass = if hasUpstream then service.upstream else null;
          recommendedProxySettings = hasUpstream;
          proxyWebsockets = hasUpstream;
          extraConfig = ''
            ${service.upstreamExtraConfig}
            ${lib.optionalString (!directWan && service.http3.enable) ''
              add_header Alt-Svc 'h3=":443"; ma=86400';
            ''}
          '';
        };
      }
      // lib.mapAttrs (_path: bypassExtraConfig: {
        proxyPass = service.upstream;
        recommendedProxySettings = hasUpstream;
        proxyWebsockets = hasUpstream;
        extraConfig = ''
          auth_request off;
          ${bypassExtraConfig}
        '';
      }) service.forwardAuthBypassPaths;
    }
    // lib.optionalAttrs directWan {
      serverName = name;
      listen = [
        {
          addr = cfg.directWan.listenAddress;
          port = cfg.directWan.listenPort;
          ssl = true;
        }
      ];
    };
in
{
  options.modules.services.ingress = {
    enable = lib.mkEnableOption "node ingress";
    legacyAcme.enable = lib.mkOption {
      type = lib.types.bool;
      default = !caddyManagesCertificates;
      description = "Temporarily retain NixOS ACME while migrating an active Caddy edge";
    };
    directWan = {
      enable = lib.mkEnableOption "direct WAN ingress listener";
      listenAddress = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Address for the direct WAN ingress listener";
      };
      listenPort = lib.mkOption {
        type = lib.types.port;
        default = 8443;
        description = "Port for the direct WAN ingress listener";
      };
    };
    forwardServices = lib.mkOption {
      default = { };
      type = lib.types.attrsOf (
        lib.types.submodule (_: {
          options = {
            upstream = lib.mkOption { type = lib.types.str; };
            acmeHost = lib.mkOption { type = lib.types.str; };
            external = lib.mkOption {
              type = lib.types.bool;
              default = false;
            };
            extraConfig = lib.mkOption {
              type = lib.types.lines;
              default = "";
            };
            upstreamExtraConfig = lib.mkOption {
              type = lib.types.lines;
              default = "";
            };
          };
        })
      );
    };
    domains = lib.mkOption {
      description = "List of ingress domains to serve";
      default = { };
      type = lib.types.attrsOf (
        lib.types.submodule (_: {
          options = {
            externalDomains = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              description = "List of domains to expose externally via the tunnel";
            };
            wildcard = {
              enable = lib.mkEnableOption "Enable wildcard domain by adding *.<domain> to the SAN";
            };
          };
        })
      );
    };
    virtualHosts = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule (_: {
          options = {
            upstream = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
            };
            acmeHost = lib.mkOption { type = lib.types.str; };
            forwardAuth = lib.mkOption {
              type = lib.types.bool;
              default = false;
            };
            forwardAuthBypassPaths = lib.mkOption {
              type = lib.types.attrsOf lib.types.lines;
              default = { };
              description = "Paths proxied without ingress forward authentication";
            };
            directWan = lib.mkOption {
              type = lib.types.bool;
              default = false;
              description = "Expose this virtual host through the direct WAN listener";
            };
            upstreamExtraConfig = lib.mkOption {
              type = lib.types.lines;
              default = "";
            };
            root = lib.mkOption {
              type = lib.types.nullOr lib.types.path;
              default = null;
            };
            http3.enable = lib.mkOption {
              type = lib.types.bool;
              default = true;
            };

            extraConfig = lib.mkOption {
              type = lib.types.lines;
              default = "";
            };
          };
        })
      );
    };
  };
  disabledModules = [
    #  "${inputs.nixpkgs-stable}/nixos/modules/services/web-servers/nginx/default.nix"
    #  "${inputs.nixpkgs-unstable}/nixos/modules/services/web-servers/nginx/default.nix"
  ];
  imports = [
    #  "${inputs.nixpkgs-mine}/nixos/modules/services/web-servers/nginx/default.nix"
  ];

  config = lib.mkMerge [
    {
      assertions = [
        {
          assertion = !hasDirectWanVirtualHosts || cfg.directWan.enable;
          message = "Direct WAN virtual hosts require modules.services.ingress.directWan.enable";
        }
        {
          assertion = !cfg.directWan.enable || cfg.enable;
          message = "The direct WAN listener requires modules.services.ingress.enable";
        }
        {
          assertion = !cfg.directWan.enable || cfg.directWan.listenAddress != null;
          message = "The direct WAN listener requires modules.services.ingress.directWan.listenAddress";
        }
      ];
    }
    (lib.mkIf cfg.enable {
      networking.firewall.allowedTCPPorts = [
        443
        8081
      ];
      services.nginx = lib.mkIf (!caddyEdgeEnabled) {
        enable = true;
        package = pkgs.nginx;
        enableReload = true;
        enableQuicBPF = true;
        defaultSSLListenPort = 443;
        defaultHTTPListenPort = 8081;
        recommendedBrotliSettings = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;
        sslCiphers = "EECDH+AESGCM:EDH+AESGCM:!aNULL";
        appendHttpConfig = ''
          map $request_uri $loggable {
            default 1;
          }
          map $http_x_request_id $req_id {
            default   $http_x_request_id;
            ""        $request_id;
          }
          log_format json_combined escape=json '{'
            '"time": $time_iso8601,'
            '"remote_addr":"$remote_addr",'
            '"status":$status,'
            '"method":"$request_method",'
            '"host":"$host",'
            '"uri":"$uri",'
            '"request_uri":"$request_uri",'
            '"request_size":$request_length,'
            '"response_size":$body_bytes_sent,'
            '"response_time":$request_time,'
            '"referrer":"$http_referer",'
            '"user_agent":"$http_user_agent",'
            '"request_id": "$req_id"'
          '}';
          log_format json_combined2 escape=json '{'
            '"time": "$time_iso8601",'
            '"remote_addr": "$proxy_protocol_addr",'
            '"x_forwarded_for": "$proxy_add_x_forwarded_for",'
            '"remote_user": "$remote_user",'
            '"bytes_sent": $bytes_sent,'
            '"request_time": $request_time,'
            '"status": $status,'
            '"vhost": "$host",'
            '"request_proto": "$server_protocol",'
            '"path": "$uri",'
            '"request_uri":"$request_uri",'
            '"request_query": "$args",'
            '"request_length": $request_length,'
            '"duration": $request_time,'
            '"method": "$request_method",'
            '"http_referrer": "$http_referer",'
            '"http_user_agent": "$http_user_agent",'
            '"upstream_addr": "$upstream_addr"'
          '}';


          access_log /var/log/nginx/access.log json_combined2  if=$loggable;
        '';
        virtualHosts =
          lib.mapAttrs' (
            name: service:
            lib.nameValuePair name {
              useACMEHost = service.acmeHost;
              forceSSL = true;
              kTLS = true;
              inherit (service) extraConfig;
              http3 = true;
              http2 = false;
              quic = true;
              locations."/" = {
                proxyPass = service.upstream;
                recommendedProxySettings = true;
                proxyWebsockets = true;
                extraConfig = ''
                  ${service.upstreamExtraConfig}
                  ${lib.optionalString true ''
                    add_header Alt-Svc 'h3=":443"; ma=86400';
                  ''}
                '';
              };
            }
          ) cfg.forwardServices
          // lib.mapAttrs (name: service: mkVirtualHost name service false) cfg.virtualHosts
          // lib.optionalAttrs (cfg.directWan.enable && cfg.directWan.listenAddress != null) (
            lib.mapAttrs' (
              name: service: lib.nameValuePair "direct-wan:${name}" (mkVirtualHost name service true)
            ) directWanVirtualHosts
            // {
              "direct-wan:default" = {
                serverName = "_direct-wan-default";
                listen = [
                  {
                    addr = cfg.directWan.listenAddress;
                    port = cfg.directWan.listenPort;
                    ssl = true;
                  }
                ];
                default = true;
                rejectSSL = true;
                http2 = false;
                http3 = false;
                quic = false;
              };
            }
          );

      };
      users.groups.acme.members = lib.mkIf cfg.legacyAcme.enable [
        (if caddyEdgeEnabled then "caddy" else "nginx")
      ];
      environment.persistence."/persist".directories = lib.mkIf cfg.legacyAcme.enable [
        "/var/lib/acme"
      ];
      sops.secrets.desec_api_token.sopsFile = ../../configs/home-ops/shared.sops.yml;
      security.acme = lib.mkIf cfg.legacyAcme.enable {
        acceptTerms = true;
        defaults = {
          email = config.repo.secrets.global.email.acme;
          credentialFiles."DESEC_TOKEN_FILE" = config.sops.secrets.desec_api_token.path;
          dnsProvider = "desec";
          environmentFile = pkgs.writeText "lego-desec.env" ''
            DESEC_PROPAGATION_TIMEOUT=700
            DESEC_POLLING_INTERVAL=20
          '';
          extraLegoFlags = [
            "--dns.resolvers"
            "ns.desec.ch:53"
            "--dns.resolvers"
            "ns.desec.cz:53"
            "--dns.resolvers"
            "ns.desec.li:53"
            "--dns.resolvers"
            "ns1.desec.io:53"
            "--dns.resolvers"
            "ns2.desec.org:53"
            "--dns-timeout"
            "30"
            "--dns.propagation-rns"
          ];
          reloadServices = [
            (if caddyEdgeEnabled then "caddy.service" else "nginx.service")
          ];
        };
        certs = lib.mapAttrs' (
          name: domain:
          (lib.nameValuePair name {
            extraDomainNames = lib.optionals domain.wildcard.enable [
              "*.${name}"
              "*.int.${name}"
            ];
          })
        ) cfg.domains;
      };
      # https://github.com/quic-go/quic-go/wiki/UDP-Buffer-Sizes
      boot.kernel.sysctl."net.core.rmem_max" = lib.mkDefault 2500000;
      boot.kernel.sysctl."net.core.wmem_max" = lib.mkDefault 2500000;
    })
  ];
}
