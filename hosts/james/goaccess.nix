{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.repo.secrets.global.domain) work;
  domain = work;
  logsDomain = "logs.${domain}";
  reportDir = "/var/lib/goaccess";
  logFile = "/var/log/nginx/access.log";

  # GoAccess configuration matching the json_combined2 nginx log format defined in web.nix.
  # Field mapping:
  #   %x  date+time (parsed with datetime-format)
  #   %h  remote host / IP (mapped from x_forwarded_for; proxy_protocol_addr is empty without PROXY protocol)
  #   %^  ignore field
  #   %b  bytes sent
  #   %T  request time in seconds
  #   %s  HTTP status
  #   %v  virtual host
  #   %H  request protocol
  #   %U  URL path
  #   %q  query string
  #   %m  HTTP method
  #   %R  referrer
  #   %u  user agent
  goaccessConf = pkgs.writeText "goaccess.conf" ''
    datetime-format %Y-%m-%dT%H:%M:%S%z
    log-format {"time": "%x","remote_addr": "%^","x_forwarded_for": "%h","remote_user": "%^","bytes_sent": %b,"request_time": %T,"status": %s,"vhost": "%v","request_proto": "%H","path": "%U","request_query": "%q","request_length": %^,"duration": %^,"method": "%m","http_referrer": "%R","http_user_agent": "%u","upstream_addr": "%^"}
  '';
in
{
  environment.persistence."/persist".directories = [
    "/var/log/nginx"
    {
      directory = reportDir;
      user = "nginx";
      group = "nginx";
      mode = "0750";
    }
  ];

  systemd.tmpfiles.rules = [
    "d '/var/log/nginx' 0750 nginx nginx - -"
  ];

  systemd.services.goaccess-report = {
    description = "Generate GoAccess nginx access log report";
    after = [ "nginx.service" ];
    unitConfig.ConditionPathExists = logFile;
    serviceConfig = {
      Type = "oneshot";
      User = "nginx";
      Group = "nginx";
      ExecStart = "${pkgs.goaccess}/bin/goaccess --config-file=${goaccessConf} --output=${reportDir}/index.html --no-progress ${logFile}";
    };
  };

  systemd.timers.goaccess-report = {
    description = "Periodic GoAccess nginx log report generation";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "2min";
      OnUnitActiveSec = "5min";
    };
  };

  # Serve the report on port 9000, accessible from Tailscale only.
  # The Tailscale CGNAT range 100.64.0.0/10 covers all tailscale node IPs.
  security.acme.certs.${logsDomain}.domain = logsDomain;
  services.nginx.virtualHosts."${logsDomain}" = {
    useACMEHost = logsDomain;
    forceSSL = true;
    kTLS = true;
    http3 = true;
    quic = true;
    extraConfig = ''
      allow 100.64.0.0/10;
      deny all;
    '';
    locations."/" = {
      root = reportDir;
      index = "index.html";
      extraConfig = "try_files $uri $uri/ =404;";
    };
  };
}
