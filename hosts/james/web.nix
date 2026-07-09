{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.repo.secrets.global) email;

in
{
  # https://github.com/quic-go/quic-go/wiki/UDP-Buffer-Sizes
  boot.kernel.sysctl."net.core.rmem_max" = lib.mkDefault 2500000;
  boot.kernel.sysctl."net.core.wmem_max" = lib.mkDefault 2500000;

  users.groups.acme.members = [ "nginx" ];
  environment.persistence."/persist".directories = [ "/var/lib/acme" ];
  sops.secrets.desec_api_token = { };
  security.acme = {
    acceptTerms = true;
    defaults = {
      email = email.acme;
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
      reloadServices = lib.optional config.services.nginx.enable "nginx";
    };
  };

  modules.zfs.datasets.properties = {
    "rpool/encrypted/safe/svc/static-web"."mountpoint" = "/var/lib/static-web";
  };

  systemd.tmpfiles.rules = [
    "z '/var/lib/static-web' 751 nginx nginx - -"
    "d '/var/lib/static-web' 750 nginx nginx - -"
    # Do not recurse ownership here; site trees can be owned by per-site deploy users.
  ];

  services.nginx = {
    enable = true;
    package = pkgs.nginx;
    enableReload = true;
    enableQuicBPF = true;
    defaultSSLListenPort = 8443;
    defaultHTTPListenPort = 8080;
    defaultListen = [
      {
        addr = "unix:/run/nginx/james-ingress.sock";
        port = null;
        ssl = true;
        proxyProtocol = true;
      }
      {
        addr = "0.0.0.0";
        port = 8080;
        ssl = false;
      }
    ];
    recommendedBrotliSettings = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

    commonHttpConfig = lib.mkForce ''
      server_names_hash_bucket_size 128;
      proxy_headers_hash_max_size 1024;
      proxy_headers_hash_bucket_size 256;
    '';
    appendHttpConfig = ''
      set_real_ip_from unix:;
      real_ip_header proxy_protocol;
      map $proxy_protocol_addr $crowdsec_client_ip {
        "" $remote_addr;
        default $proxy_protocol_addr;
      }
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
      log_format crowdsec_combined '$crowdsec_client_ip - $remote_user [$time_local] "$request" '
        '$status $body_bytes_sent "$http_referer" "$http_user_agent"';
      access_log /var/log/nginx/crowdsec.log crowdsec_combined if=$loggable;
      access_log /var/log/nginx/access.log json_combined2  if=$loggable;
    '';
    virtualHosts = {
      "_" = {
        default = true;
        extraConfig = ''
          return 404;
        '';
      };
    };
  };
}
