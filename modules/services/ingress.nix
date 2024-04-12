{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.services.ingress;
  domainsWithTunnel = lib.filterAttrs (name: domain: domain.tunnel.enable) cfg.domains;
in
{
  options.modules.services.ingress = {
    enable = lib.mkEnableOption "node ingress";
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
              tunnel = {
                enable = lib.mkEnableOption "Enable cloudflared tunnel";
                tunnelId = lib.mkOption {
                  type = lib.types.str;
                  description = "The tunnel ID for the cloudflared tunnel";
                };
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
  # TODO(24.05): until pr 277189 lands in stable we have to use the unstable cloudflared nixos module
  # ref: https://nixpk.gs/pr-tracker.html?pr=277189
  disabledModules = [
    "${inputs.nixpkgs-stable}/nixos/modules/services/networking/cloudflared.nix"
    "${inputs.nixpkgs-stable}/nixos/modules/services/web-servers/nginx/default.nix"
    "${inputs.nixpkgs-unstable}/nixos/modules/services/web-servers/nginx/default.nix"
  ];
  imports = [
    "${inputs.nixpkgs-unstable}/nixos/modules/services/networking/cloudflared.nix"
    "${inputs.nixpkgs-mine}/nixos/modules/services/web-servers/nginx/default.nix"
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
      virtualHosts = lib.mapAttrs' (
        name: service:
        lib.nameValuePair name {
          useACMEHost = service.acmeHost;
          forceSSL = true;
          kTLS = true;
          extraConfig = service.extraConfig;
          http3 = true;
          http2 = false;
          quic = true;
          locations = {
            "/" = {
              proxyPass = service.upstream;
              recommendedProxySettings = true;
              extraConfig = ''
                ${service.upstreamExtraConfig}
                 add_header Alt-Svc 'h3=":443"; ma=86400';
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
                add_header              Set-Cookie $auth_cookie;
                auth_request_set        $auth_cookie $upstream_http_set_cookie;
                proxy_pass_request_body off;
                proxy_set_header        Content-Length "";
              '';
            };
            "@goauthentik_proxy_signin" = lib.mkIf service.forwardAuth {
              extraConfig = ''
                internal;
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
        CF_DNS_API_TOKEN=${config.sops.placeholder."acmeSecrets/cloudflareDnsToken"}
      '';
    };

    users.groups.acme.members = [ "nginx" ];
    environment.persistence."/persist".directories = [ "/var/lib/acme" ];
    security.acme = {
      acceptTerms = true;
      defaults = {
        email = config.repo.secrets.global.domain.acme.email;
        credentialsFile = config.sops.templates.acme-credentials.path;
        dnsProvider = "cloudflare";
        dnsPropagationCheck = true;
        extraLegoFlags = [ "--dns.resolvers=1.1.1.1:53" ];
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

    sops.secrets =
      lib.mapAttrs' (
        name: domain: lib.nameValuePair "cloudflareTunnels/${name}" { mode = "400"; }
      ) domainsWithTunnel
      // {
        "acmeSecrets/cloudflareDnsToken" = {
          sopsFile = ../../configs/home-ops/shared.sops.yml;
        };
      };
    # https://github.com/quic-go/quic-go/wiki/UDP-Buffer-Sizes
    boot.kernel.sysctl."net.core.rmem_max" = lib.mkDefault 2500000;
    boot.kernel.sysctl."net.core.wmem_max" = lib.mkDefault 2500000;
    systemd.services = lib.mapAttrs' (
      name: domain:
      lib.nameValuePair "cloudflared-tunnel-${domain.tunnel.tunnelId}" {
        preStart = ''
          rm -f $STATE_DIRECTORY/credentials.json
          cat $CREDENTIALS_DIRECTORY/CREDENTIALS > $STATE_DIRECTORY/credentials.json
        '';
        serviceConfig = {
          UMask = 77;
          User = lib.mkForce null;
          Group = lib.mkForce null;
          DynamicUser = true;
          LoadCredential = [ "CREDENTIALS:${config.sops.secrets."cloudflareTunnels/${name}".path}" ];
          StateDirectory = "cloudflared-tunnel/${domain.tunnel.tunnelId}";
          LockPersonality = true;
          NoNewPrivileges = true;
          PrivateDevices = true;
          PrivateMounts = true;
          PrivateTmp = true;
          ProtectSystem = "strict";
          ProtectHome = true;
          ProtectControlGroups = true;
          RestrictAddressFamilies = "AF_UNIX AF_INET AF_INET6";
          ProtectClock = true;
          ProtectProc = "invisible";
          ProtectHostname = true;
          ProtectKernelLogs = true;
          ProtectKernelModules = true;
          ProtectKernelTunables = true;
          RemoveIPC = true;
          RestrictNamespaces = true;
          RestrictRealtime = true;
          RestrictSUIDSGID = true;
          SystemCallFilter = [
            "@system-service"
            "~@privileged"
            "@resources"
          ];
          SystemCallArchitectures = "native";
          MemoryDenyWriteExecute = true;
        };
      }
    ) domainsWithTunnel;
    services.cloudflared = {
      enable = true;
      tunnels = lib.mapAttrs' (
        name: domain:
        lib.nameValuePair domain.tunnel.tunnelId {
          #credentialsFile = config.sops.secrets."cloudflareTunnels/${name}".path;
          credentialsFile = "/var/lib/cloudflared-tunnel/${domain.tunnel.tunnelId}/credentials.json";
          originRequest.originServerName = "localhost";
          originRequest.noTLSVerify = true;
          ingress = lib.foldl' (
            acc: extDomain: acc // { "${extDomain}" = "https://localhost:443"; }
          ) { } domain.externalDomains;
          # Catch-all if no match
          default = "http_status:404";
        }
      ) domainsWithTunnel;
    };
  };
}
