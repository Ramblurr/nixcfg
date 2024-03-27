{ config, lib, pkgs, ... }: {
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    commonHttpConfig = lib.mkForce ''
      server_names_hash_bucket_size 128;
      proxy_headers_hash_max_size 1024;
      proxy_headers_hash_bucket_size 256;
    '';
    virtualHosts = {
      # attic nix cache endpoint
      "attic.mgmt.***REMOVED***" = {
        useACMEHost = "attic.mgmt.***REMOVED***";
        forceSSL = true;
        http3 = false;
        http2 = false;
        kTLS = true;
        extraConfig = ''
          client_max_body_size 0;
          client_header_buffer_size 64k;
        '';
        locations."/" = {
          proxyPass = "http://127.0.0.1:57000";
          recommendedProxySettings = true;
        };
      };
      # minio s3 endpoint
      "s3.data.***REMOVED***" = {
        useACMEHost = "s3.data.***REMOVED***";
        serverAliases = [ "s3.mgmt.***REMOVED***" ];
        forceSSL = true;
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
          allow 10.9.10.0/23;
          allow 10.9.8.0/23;
          deny all;
        '';
      };
      # minio admin console
      "minio.data.***REMOVED***" = {
        serverAliases = [ "minio.mgmt.***REMOVED***" ];
        forceSSL = true;
        useACMEHost = "s3.data.***REMOVED***";
        extraConfig = ''
          # To allow special characters in headers
          ignore_invalid_headers off;
          # Allow any size file to be uploaded.
          # Set to a value such as 1000m; to restrict file size to a specific value
          client_max_body_size 0;
          # To disable buffering
          proxy_buffering off;
          allow 10.9.10.0/23;
          allow 10.9.8.0/23;
          deny all;
        '';
        locations = {
          "/" = {
            proxyPass = "http://127.0.0.1:8999";
            extraConfig = ''
              proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-NginX-Proxy true;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto $scheme;
              # proxy_set_header Host $http_host;
              # proxy_set_header Host $host;
              # This is necessary to pass the correct IP to be hashed
              real_ip_header X-Real-IP;
              proxy_connect_timeout 300;
              # To support websocket
              proxy_http_version 1.1;
              proxy_set_header Upgrade $http_upgrade;
              proxy_set_header Connection "upgrade";
            '';
          };
          "/minio/ui/" = {
            proxyPass = "http://127.0.0.1:8999";
            extraConfig = ''
              rewrite ^/minio/ui/(.*) /$1 break;
              proxy_set_header Host $host;
              proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto $scheme;
              proxy_set_header X-NginX-Proxy true;

              # This is necessary to pass the correct IP to be hashed
              real_ip_header X-Real-IP;

              proxy_connect_timeout 300;

              # To support websockets in MinIO versions released after January 2023
              proxy_http_version 1.1;
              proxy_set_header Upgrade $http_upgrade;
              proxy_set_header Connection "upgrade";

              chunked_transfer_encoding off;
            '';
          };
        };
      };
    };
  };
}
