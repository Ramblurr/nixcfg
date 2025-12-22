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

  sops.templates.acme-credentials.content = ''
    BUNNY_API_KEY=${config.sops.placeholder.bunnyApiKey}
  '';

  sops.secrets.bunnyApiKey = {
    #restartUnits = [ ];
  };

  users.groups.acme.members = [ "nginx" ];
  environment.persistence."/persist".directories = [ "/var/lib/acme" ];
  security.acme = {
    acceptTerms = true;
    defaults = {
      email = email.acme;
      dnsProvider = "bunny";
      dnsPropagationCheck = true;
      credentialsFile = config.sops.templates.acme-credentials.path;
      reloadServices = lib.optional config.services.nginx.enable "nginx";
    };
  };

  modules.zfs.datasets.properties = {
    "rpool/encrypted/safe/svc/static-web"."mountpoint" = "/var/lib/static-web";
  };

  systemd.tmpfiles.rules = [
    "z '/var/lib/static-web' 751 nginx nginx - -"
    "d '/var/lib/static-web' 750 nginx nginx - -"
    "Z '/var/lib/static-web' 750 nginx nginx - -"
  ];

  services.nginx = {
    enable = true;
    package = pkgs.nginx;
    enableReload = true;
    enableQuicBPF = true;
    defaultSSLListenPort = 8443;
    defaultHTTPListenPort = 8080;
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
