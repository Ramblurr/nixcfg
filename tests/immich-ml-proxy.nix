{ pkgs, ... }:
let
  certificates =
    pkgs.runCommand "immich-ml-proxy-test-certificates"
      {
        nativeBuildInputs = [ pkgs.openssl ];
      }
      ''
        mkdir -p "$out"
        openssl req -x509 -newkey rsa:2048 -nodes -days 365 \
          -subj /CN=immich-ml-test-ca \
          -keyout "$out/ca-key.pem" -out "$out/ca.pem" >/dev/null 2>&1
        openssl req -x509 -newkey rsa:2048 -nodes -days 1 \
          -subj /CN=wrong-immich-ml-test-ca \
          -keyout "$out/wrong-ca-key.pem" -out "$out/wrong-ca.pem" >/dev/null 2>&1

        issue_certificate() {
          name=$1
          extended_key_usage=$2
          subject_alt_name=$3
          validity_days=$4
          openssl req -newkey rsa:2048 -nodes -subj "/CN=$name" \
            -keyout "$out/$name-key.pem" -out "$out/$name.csr" >/dev/null 2>&1
          {
            printf 'basicConstraints=CA:FALSE\n'
            printf 'keyUsage=digitalSignature,keyEncipherment\n'
            printf 'extendedKeyUsage=%s\n' "$extended_key_usage"
            if [ -n "$subject_alt_name" ]; then
              printf 'subjectAltName=%s\n' "$subject_alt_name"
            fi
          } > "$out/$name.ext"
          openssl x509 -req -days "$validity_days" -sha256 \
            -in "$out/$name.csr" -CA "$out/ca.pem" -CAkey "$out/ca-key.pem" \
            -CAcreateserial -extfile "$out/$name.ext" -out "$out/$name.pem" >/dev/null 2>&1
        }

        issue_certificate server serverAuth DNS:immich-ml.test 40
        issue_certificate wrong-server serverAuth DNS:wrong-name.test 40
        issue_certificate authorized-client clientAuth "" 40
        issue_certificate replacement-client clientAuth "" 40
        issue_certificate expiring-client clientAuth "" 1
        issue_certificate expired-client clientAuth "" 0
        issue_certificate rogue-client clientAuth "" 40
      '';
  provisionCertificates = serviceName: {
    systemd.services.immich-ml-test-certificates = {
      wantedBy = [ "multi-user.target" ];
      before = [ "${serviceName}.service" ];
      serviceConfig = {
        RemainAfterExit = true;
        Type = "oneshot";
      };
      script = ''
        install -d -m 0700 /run/immich-ml-test-certificates
        install -m 0444 ${certificates}/ca.pem /run/immich-ml-test-certificates/ca.pem
        install -m 0444 ${certificates}/server.pem /run/immich-ml-test-certificates/server.pem
        install -m 0400 ${certificates}/server-key.pem /run/immich-ml-test-certificates/server-key.pem
        install -m 0444 ${certificates}/wrong-server.pem /run/immich-ml-test-certificates/wrong-server.pem
        install -m 0400 ${certificates}/wrong-server-key.pem /run/immich-ml-test-certificates/wrong-server-key.pem
        install -m 0444 ${certificates}/authorized-client.pem /run/immich-ml-test-certificates/authorized-client.pem
        install -m 0400 ${certificates}/authorized-client-key.pem /run/immich-ml-test-certificates/authorized-client-key.pem
        install -m 0444 ${certificates}/rogue-client.pem /run/immich-ml-test-certificates/rogue-client.pem
        install -m 0400 ${certificates}/rogue-client-key.pem /run/immich-ml-test-certificates/rogue-client-key.pem
        install -m 0444 ${certificates}/replacement-client.pem /run/immich-ml-test-certificates/replacement-client.pem
        install -m 0400 ${certificates}/replacement-client-key.pem /run/immich-ml-test-certificates/replacement-client-key.pem
        install -m 0444 ${certificates}/expiring-client.pem /run/immich-ml-test-certificates/expiring-client.pem
        install -m 0444 ${certificates}/expired-client.pem /run/immich-ml-test-certificates/expired-client.pem
        install -m 0400 ${certificates}/expired-client-key.pem /run/immich-ml-test-certificates/expired-client-key.pem
        install -m 0444 ${certificates}/wrong-ca.pem /run/immich-ml-test-certificates/wrong-ca.pem
        printf 'synthetic-token\n' > /run/immich-ml-test-certificates/gatus-token
        chmod 0400 /run/immich-ml-test-certificates/gatus-token
      '';
    };
    systemd.services.${serviceName} = {
      requires = [ "immich-ml-test-certificates.service" ];
      after = [ "immich-ml-test-certificates.service" ];
    };
  };
in
pkgs.testers.runNixOSTest {
  name = "immich-ml-proxy";

  nodes = {
    server =
      { pkgs, ... }:
      {
        imports = [
          ../modules/services/immich-ml-proxy.nix
          (provisionCertificates "immich-ml-server-proxy")
        ];
        environment.systemPackages = [ pkgs.curl ];
        networking.firewall.enable = true;
        systemd.services.immich-ml-test-backend = {
          wantedBy = [ "multi-user.target" ];
          serviceConfig = {
            ExecStart = "${pkgs.python3}/bin/python -m http.server 3003 --bind 127.0.0.1";
            DynamicUser = true;
          };
        };
        modules.services.immich-ml-proxy = {
          enable = true;
          role = "server";
          listenAddress = "0.0.0.0";
          serverName = "immich-ml.test";
          allowedSourceAddresses = [
            "192.168.1.1"
            "2001:db8:1::1"
          ];
          credentials = {
            ca = "/run/immich-ml-test-certificates/ca.pem";
            certificate = "/run/immich-ml-test-certificates/server.pem";
            privateKey = "/run/immich-ml-test-certificates/server-key.pem";
          };
          authorizedClientCertificates."authorized-client.pem" =
            "/run/immich-ml-test-certificates/authorized-client.pem";
          authorizedClientCertificates."expired-client.pem" =
            "/run/immich-ml-test-certificates/expired-client.pem";
          monitoredCertificates = {
            "server.pem" = "/run/immich-ml-test-certificates/server.pem";
            "authorized-client.pem" = "/run/immich-ml-test-certificates/authorized-client.pem";
            "expiring-client.pem" = "/run/immich-ml-test-certificates/expiring-client.pem";
          };
        };
        systemd.services.immich-ml-server-proxy-certificate-expiry.serviceConfig.LoadCredential = [
          "gatus-token:/run/immich-ml-test-certificates/gatus-token"
        ];
      };

    client =
      { pkgs, ... }:
      {
        imports = [
          ../modules/services/immich-ml-proxy.nix
          (provisionCertificates "immich-ml-client-proxy")
        ];
        environment.systemPackages = [ pkgs.curl ];
        users.users = {
          immich = {
            isSystemUser = true;
            group = "immich";
            uid = 3024;
          };
          unrelated = {
            isSystemUser = true;
            group = "unrelated";
            uid = 3025;
          };
        };
        users.groups = {
          immich.gid = 3024;
          unrelated.gid = 3025;
        };
        modules.services.immich-ml-proxy = {
          enable = true;
          role = "client";
          upstreamAddress = "192.168.1.2";
          upstreamPort = 3443;
          serverName = "immich-ml.test";
          allowedUser = "immich";
          credentials = {
            ca = "/run/immich-ml-test-certificates/ca.pem";
            certificate = "/run/immich-ml-test-certificates/authorized-client.pem";
            privateKey = "/run/immich-ml-test-certificates/authorized-client-key.pem";
          };
        };
      };
  };

  testScript = ''
    start_all()
    server.wait_for_unit("immich-ml-server-proxy.service")
    server.wait_for_unit("immich-ml-test-backend.service")
    client.wait_for_unit("immich-ml-client-proxy.service")
    server.fail("systemctl start immich-ml-server-proxy-certificate-expiry.service")
    server.succeed("systemctl is-failed immich-ml-server-proxy-certificate-expiry.service")
    server.succeed("install -m 0444 /run/immich-ml-test-certificates/replacement-client.pem /run/immich-ml-test-certificates/expiring-client.pem")
    server.succeed("systemctl reset-failed immich-ml-server-proxy-certificate-expiry.service")
    server.succeed("systemctl start immich-ml-server-proxy-certificate-expiry.service")

    client.succeed("runuser -u immich -- curl --noproxy '*' --fail --silent --max-time 10 http://127.0.0.1:3004/ >/dev/null")

    client.succeed("install -m 0444 /run/immich-ml-test-certificates/wrong-ca.pem /run/immich-ml-test-certificates/ca.pem")
    client.succeed("systemctl restart immich-ml-client-proxy.service")
    client.fail("runuser -u immich -- curl --noproxy '*' --fail --silent --max-time 5 http://127.0.0.1:3004/ >/dev/null")
    client.succeed("install -m 0444 ${certificates}/ca.pem /run/immich-ml-test-certificates/ca.pem")
    client.succeed("systemctl restart immich-ml-client-proxy.service")
    client.wait_until_succeeds("runuser -u immich -- curl --noproxy '*' --fail --silent --max-time 5 http://127.0.0.1:3004/ >/dev/null", timeout=5)

    server.succeed("install -m 0444 /run/immich-ml-test-certificates/wrong-server.pem /run/immich-ml-test-certificates/server.pem")
    server.succeed("install -m 0400 /run/immich-ml-test-certificates/wrong-server-key.pem /run/immich-ml-test-certificates/server-key.pem")
    server.succeed("systemctl restart immich-ml-server-proxy.service")
    client.fail("runuser -u immich -- curl --noproxy '*' --fail --silent --max-time 5 http://127.0.0.1:3004/ >/dev/null")
    server.succeed("install -m 0444 ${certificates}/server.pem /run/immich-ml-test-certificates/server.pem")
    server.succeed("install -m 0400 ${certificates}/server-key.pem /run/immich-ml-test-certificates/server-key.pem")
    server.succeed("systemctl restart immich-ml-server-proxy.service")
    client.wait_until_succeeds("runuser -u immich -- curl --noproxy '*' --fail --silent --max-time 5 http://127.0.0.1:3004/ >/dev/null", timeout=5)
    client.fail("runuser -u unrelated -- curl --noproxy '*' --fail --silent --max-time 2 http://127.0.0.1:3004/ >/dev/null")
    client.fail("runuser -u unrelated -- curl --noproxy '*' --fail --silent --max-time 2 http://[::1]:3004/ >/dev/null")

    client.fail("curl --noproxy '*' --fail --silent --max-time 5 --connect-to immich-ml.test:3443:192.168.1.2:3443 --cacert /run/immich-ml-test-certificates/ca.pem https://immich-ml.test:3443/ >/dev/null")
    client.fail("curl --noproxy '*' --fail --silent --max-time 5 --connect-to immich-ml.test:3443:192.168.1.2:3443 --cacert /run/immich-ml-test-certificates/ca.pem --cert /run/immich-ml-test-certificates/rogue-client.pem --key /run/immich-ml-test-certificates/rogue-client-key.pem https://immich-ml.test:3443/ >/dev/null")
    client.fail("curl --noproxy '*' --fail --silent --max-time 5 --connect-to wrong-name.test:3443:192.168.1.2:3443 --cacert /run/immich-ml-test-certificates/ca.pem --cert /run/immich-ml-test-certificates/authorized-client.pem --key /run/immich-ml-test-certificates/authorized-client-key.pem https://wrong-name.test:3443/ >/dev/null")
    client.fail("curl --noproxy '*' --fail --silent --max-time 5 --connect-to immich-ml.test:3443:192.168.1.2:3443 --cacert /run/immich-ml-test-certificates/wrong-ca.pem --cert /run/immich-ml-test-certificates/authorized-client.pem --key /run/immich-ml-test-certificates/authorized-client-key.pem https://immich-ml.test:3443/ >/dev/null")
    client.fail("curl --noproxy '*' --fail --silent --max-time 5 --connect-to immich-ml.test:3443:192.168.1.2:3443 --cacert /run/immich-ml-test-certificates/ca.pem --cert /run/immich-ml-test-certificates/expired-client.pem --key /run/immich-ml-test-certificates/expired-client-key.pem https://immich-ml.test:3443/ >/dev/null")
    client.succeed("ip address add 192.168.1.3/24 dev eth1")
    client.fail("curl --interface 192.168.1.3 --noproxy '*' --fail --silent --max-time 2 --connect-to immich-ml.test:3443:192.168.1.2:3443 --cacert /run/immich-ml-test-certificates/ca.pem --cert /run/immich-ml-test-certificates/authorized-client.pem --key /run/immich-ml-test-certificates/authorized-client-key.pem https://immich-ml.test:3443/ >/dev/null")
    client.succeed("curl --noproxy '*' --fail --silent --max-time 5 --connect-to immich-ml.test:3443:192.168.1.2:3443 --cacert /run/immich-ml-test-certificates/ca.pem --cert /run/immich-ml-test-certificates/authorized-client.pem --key /run/immich-ml-test-certificates/authorized-client-key.pem https://immich-ml.test:3443/ >/dev/null")

    server.succeed("install -m 0444 /run/immich-ml-test-certificates/replacement-client.pem /run/immich-ml-test-certificates/authorized-client.pem")
    server.succeed("systemctl restart immich-ml-server-proxy.service")
    client.fail("curl --noproxy '*' --fail --silent --max-time 5 --connect-to immich-ml.test:3443:192.168.1.2:3443 --cacert /run/immich-ml-test-certificates/ca.pem --cert /run/immich-ml-test-certificates/authorized-client.pem --key /run/immich-ml-test-certificates/authorized-client-key.pem https://immich-ml.test:3443/ >/dev/null")
    client.fail("runuser -u immich -- curl --noproxy '*' --fail --silent --max-time 5 http://127.0.0.1:3004/ >/dev/null")
    client.succeed("install -m 0444 /run/immich-ml-test-certificates/replacement-client.pem /run/immich-ml-test-certificates/authorized-client.pem")
    client.succeed("install -m 0400 /run/immich-ml-test-certificates/replacement-client-key.pem /run/immich-ml-test-certificates/authorized-client-key.pem")
    client.succeed("systemctl restart immich-ml-client-proxy.service")
    client.wait_until_succeeds("runuser -u immich -- curl --noproxy '*' --fail --silent --max-time 5 http://127.0.0.1:3004/ >/dev/null", timeout=5)
    client.succeed("curl --noproxy '*' --fail --silent --max-time 5 --connect-to immich-ml.test:3443:192.168.1.2:3443 --cacert /run/immich-ml-test-certificates/ca.pem --cert /run/immich-ml-test-certificates/replacement-client.pem --key /run/immich-ml-test-certificates/replacement-client-key.pem https://immich-ml.test:3443/ >/dev/null")
  '';
}
