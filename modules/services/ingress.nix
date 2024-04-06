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
          '"http_user_agent": "$http_user_agent"'
        '}';


        access_log /var/log/nginx/access.log json_combined2  if=$loggable;
      '';
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
          extraDomainNames = lib.optionals domain.wildcard.enable [ "*.${name}" ];
        })
      ) cfg.domains;
    };

    sops.secrets =
      lib.mapAttrs' (
        name: domain:
        lib.nameValuePair "cloudflareTunnels/${name}" {
          mode = "400";
          owner = config.services.cloudflared.user;
        }
      ) domainsWithTunnel
      // {
        "acmeSecrets/cloudflareDnsToken" = { };
      };
    # First time setup:
    # cloudflared tunnel login
    # cloudflared tunnel create <ingressDomain>
    # edit secrets files
    # Remove ~/.cloudflared/
    services.cloudflared = {
      enable = true;
      tunnels = lib.mapAttrs' (
        name: domain:
        lib.nameValuePair domain.tunnel.tunnelId {
          credentialsFile = config.sops.secrets."cloudflareTunnels/${name}".path;
          originRequest.originServerName = "${config.networking.fqdn}";
          ingress = lib.foldl' (
            acc: extDomain: acc // { "${extDomain}" = "https://${config.networking.fqdn}:443"; }
          ) { } domain.externalDomains;
          # Catch-all if no match
          default = "http_status:404";
        }
      ) domainsWithTunnel;
    };
  };
}
