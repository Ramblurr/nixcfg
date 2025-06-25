{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.services.ingress;
in
{
  options.modules.services.ingress = {
    enable = lib.mkEnableOption "node ingress";
    forwardServices = lib.mkOption {
      default = { };
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }:
          {
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
          }
        )
      );
    };
    domains = lib.mkOption {
      description = "List of ingress domains to serve";
      default = { };
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }:
          {
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
          }
        )
      );
    };
    virtualHosts = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }:
          {
            options = {
              upstream = lib.mkOption { type = lib.types.str; };
              acmeHost = lib.mkOption { type = lib.types.str; };
              forwardAuth = lib.mkOption {
                type = lib.types.bool;
                default = false;
              };
              upstreamExtraConfig = lib.mkOption {
                type = lib.types.lines;
                default = "";
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
          }
        )
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

  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [
      443
      8081
    ];
    services.nginx = {
      enable = true;
      package = pkgs.nginxQuic;
      enableReload = true;
      enableQuicBPF = true;
      defaultSSLListenPort = 443;
      defaultHTTPListenPort = 8081;
      recommendedBrotliSettings = true;
      recommendedGzipSettings = true;
      recommendedZstdSettings = true;
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
          '"uri":"$request_uri",'
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
            extraConfig = service.extraConfig;
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
        // lib.mapAttrs' (
          name: service:
          lib.nameValuePair name {
            useACMEHost = service.acmeHost;
            forceSSL = true;
            kTLS = true;
            extraConfig = service.extraConfig;
            http3 = service.http3.enable;
            http2 = false;
            quic = service.http3.enable;
            locations = {
              "/" = {
                proxyPass = service.upstream;
                recommendedProxySettings = true;
                proxyWebsockets = true;
                extraConfig = ''
                  ${service.upstreamExtraConfig}
                  ${lib.optionalString service.http3.enable ''
                    add_header Alt-Svc 'h3=":443"; ma=86400';
                  ''}
                  ${lib.optionalString service.forwardAuth ''
                    auth_request        /outpost.goauthentik.io/auth/nginx;
                    error_page          401 = @goauthentik_proxy_signin;
                    auth_request_set $auth_cookie $upstream_http_set_cookie;
                    add_header Set-Cookie $auth_cookie;

                    # translate headers from the outposts back to the actual upstream
                    auth_request_set $authentik_username $upstream_http_x_authentik_username;
                    auth_request_set $authentik_groups $upstream_http_x_authentik_groups;
                    auth_request_set $authentik_email $upstream_http_x_authentik_email;
                    auth_request_set $authentik_name $upstream_http_x_authentik_name;
                    auth_request_set $authentik_uid $upstream_http_x_authentik_uid;

                    proxy_set_header X-authentik-username $authentik_username;
                    proxy_set_header X-authentik-groups $authentik_groups;
                    proxy_set_header X-authentik-email $authentik_email;
                    proxy_set_header X-authentik-name $authentik_name;
                    proxy_set_header X-authentik-uid $authentik_uid;
                  ''}
                '';
              };
              "/outpost.goauthentik.io" = lib.mkIf service.forwardAuth {
                extraConfig = ''
                  proxy_pass              http://127.0.0.1:${toString config.modules.services.authentik.ports.http}/outpost.goauthentik.io;
                  proxy_set_header        Host $host;
                  proxy_set_header        X-Original-URL $scheme://$http_host$request_uri;
                  auth_request_set        $auth_cookie $upstream_http_set_cookie;
                  add_header              Set-Cookie $auth_cookie;
                  proxy_pass_request_body off;
                  proxy_set_header        Content-Length "";
                '';
              };
              "@goauthentik_proxy_signin" = lib.mkIf service.forwardAuth {
                extraConfig = ''
                  internal;
                  auth_request_set $auth_cookie $upstream_http_set_cookie;
                  add_header Set-Cookie $auth_cookie;
                  return 302 /outpost.goauthentik.io/start?rd=$request_uri;
                  # For domain level, use the below error_page to redirect to your authentik server with the full redirect path
                  # return 302 https://auth.${service.acmeHost}/outpost.goauthentik.io/start?rd=$scheme://$http_host$request_uri;
                '';
              };
            };
          }
        ) cfg.virtualHosts;

      # This is selected when no matching host is found for a request.
      #virtualHosts."\"\"" = {
      #  useACMEHost = cfg.ingress.domain;
      #  onlySSL = true;
      #  kTLS = true;
      #  extraConfig = ''
      #    return 404;
      #  '';
      #};
    };
    sops.templates.acme-credentials = {
      mode = "440";
      group = "acme";
      content = ''
        BUNNY_API_KEY=${config.sops.placeholder.bunnyApiKey}
      '';
    };

    users.groups.acme.members = [ "nginx" ];
    environment.persistence."/persist".directories = [ "/var/lib/acme" ];
    security.acme = {
      acceptTerms = true;
      defaults = {
        email = config.repo.secrets.global.email.acme;
        credentialsFile = config.sops.templates.acme-credentials.path;
        dnsProvider = "bunny";
        dnsPropagationCheck = false;
        reloadServices = [ "nginx.service" ];
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

    sops.secrets.bunnyApiKey = {
      sopsFile = ../../configs/home-ops/shared.sops.yml;
    };
    # https://github.com/quic-go/quic-go/wiki/UDP-Buffer-Sizes
    boot.kernel.sysctl."net.core.rmem_max" = lib.mkDefault 2500000;
    boot.kernel.sysctl."net.core.wmem_max" = lib.mkDefault 2500000;
  };
}
