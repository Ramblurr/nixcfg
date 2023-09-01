{
  config,
  lib,
  pkgs,
  ...
}: {
  networking.firewall.allowedTCPPorts = [80 443];
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts = {
      # minio s3 endpoint
      "s3.data.socozy.casa" = {
        useACMEHost = "s3.data.socozy.casa";
        addSSL = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:9000";
          extraConfig = ''
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
            # proxy_set_header Host $host;
            proxy_connect_timeout 300;
            # Default is HTTP/1, keepalive is only enabled in HTTP/1.1
            proxy_http_version 1.1;
            proxy_set_header Connection "";
            chunked_transfer_encoding off;
          '';
        };
        extraConfig = ''
          # To allow special characters in headers
          ignore_invalid_headers off;
          # Allow any size file to be uploaded.
          # Set to a value such as 1000m; to restrict file size to a specific value
          client_max_body_size 0;
          # To disable buffering
          proxy_buffering off;
        '';
      };
      # minio admin console
      "minio.data.socozy.casa" = {
        addSSL = true;
        useACMEHost = "s3.data.socozy.casa";
        extraConfig = ''
          # To allow special characters in headers
          ignore_invalid_headers off;
          # Allow any size file to be uploaded.
          # Set to a value such as 1000m; to restrict file size to a specific value
          client_max_body_size 0;
          # To disable buffering
          proxy_buffering off;
        '';
        locations = {
          "/" = {
            proxyPass = "http://127.0.0.1:8999";
            extraConfig = ''
              proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto $scheme;
              # proxy_set_header Host $host;
              proxy_connect_timeout 300;
              # Default is HTTP/1, keepalive is only enabled in HTTP/1.1
              proxy_http_version 1.1;
              proxy_set_header Connection "";
              chunked_transfer_encoding off;
            '';
          };
        };
      };
    };
  };
}
