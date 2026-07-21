{ inputs, pkgs }:

let
  testSecrets = ./fixtures/hindsight.sops.yaml;
  testHindsightServer = pkgs.writeShellApplication {
    name = "hindsight-test-server";
    runtimeInputs = [ pkgs.busybox ];
    text = ''
      mkdir -p /tmp/hindsight-test
      printf '%s\n' healthy > /tmp/hindsight-test/health
      printf '%s\n' hindsight-control-plane > /tmp/hindsight-test/index.html
      httpd -f -p 8888 -h /tmp/hindsight-test &
      httpd -f -p 9999 -h /tmp/hindsight-test &
      wait
    '';
  };

  testHindsightImage = pkgs.dockerTools.buildLayeredImage {
    name = "hindsight-test";
    tag = "latest";
    contents = [
      pkgs.busybox
      pkgs.curl
      testHindsightServer
    ];
    config = {
      Entrypoint = [ "${testHindsightServer}/bin/hindsight-test-server" ];
      Env = [
        "PATH=${
          pkgs.lib.makeBinPath [
            pkgs.busybox
            pkgs.curl
          ]
        }"
      ];
    };
  };

  testPostgresServer = pkgs.writeShellApplication {
    name = "hindsight-test-postgres";
    runtimeInputs = [
      pkgs.busybox
      pkgs.gnugrep
    ];
    text = ''
      test "$POSTGRES_USER" = hindsight
      test "$POSTGRES_DB" = hindsight
      test "$POSTGRES_PASSWORD" = test-postgres-password
      grep -Fxq 'CREATE EXTENSION IF NOT EXISTS vector;' \
        /docker-entrypoint-initdb.d/10-hindsight.sql
      mkdir -p /var/lib/postgresql/18/docker
      printf '%s\n' initialized > /var/lib/postgresql/18/docker/hindsight-test
      exec sleep infinity
    '';
  };

  testPgReady = pkgs.writeShellApplication {
    name = "pg_isready";
    text = "exit 0";
  };

  testPostgresImage = pkgs.dockerTools.buildLayeredImage {
    name = "hindsight-postgres-test";
    tag = "latest";
    contents = [
      pkgs.busybox
      pkgs.gnugrep
      testPgReady
      testPostgresServer
    ];
    config = {
      Entrypoint = [ "${testPostgresServer}/bin/hindsight-test-postgres" ];
      Env = [
        "PATH=${
          pkgs.lib.makeBinPath [
            pkgs.busybox
            pkgs.gnugrep
            testPgReady
          ]
        }"
      ];
    };
  };
in
pkgs.testers.runNixOSTest {
  name = "hindsight-rootless";

  nodes.machine =
    {
      config,
      lib,
      options,
      ...
    }:
    let
      app = config.virtualisation.quadlet.containers.hindsight;
      database = config.virtualisation.quadlet.containers.hindsight-db;
      network = config.virtualisation.quadlet.networks.hindsight;
      volume = config.virtualisation.quadlet.volumes.hindsight-db-data;
      ingress = config.modules.services.ingress.virtualHosts."hindsight.socozy.casa";
      appTemplate = config.sops.templates."hindsight-app.env";
      dbTemplate = config.sops.templates."hindsight-db.env";
    in
    {
      imports = [
        inputs.impermanence.nixosModules.impermanence
        inputs.quadlet-nix2.nixosModules.default
        inputs.sops-nix.nixosModules.sops
        ../modules/services/ingress.nix
        ../modules/services/hindsight.nix
      ];

      networking.hostName = "debord";
      system.stateVersion = "26.05";

      fileSystems."/persist" = {
        device = "tmpfs";
        fsType = "tmpfs";
        neededForBoot = true;
      };

      modules.services.hindsight = {
        enable = true;
        domain = "hindsight.socozy.casa";
        acmeHost = "socozy.casa";
        image = "docker-archive:${testHindsightImage}";
        postgresImage = "docker-archive:${testPostgresImage}";
      };

      sops = {
        defaultSopsFile = testSecrets;
        age.keyFile = "/etc/sops/age/keys.txt";
      };

      # This deliberately committed test-only key is split so secret scanning still
      # rejects any complete age private key accidentally added to the repository.
      environment.etc."sops/age/keys.txt".text =
        "AGE-SECRET-" + "KEY-17234342FN8EHCK6G4REV520TDFALHLTWDV9YHYLK72XEY3VY2XRS4JWM6A\n";

      assertions = [
        {
          assertion = lib.all (object: object.uid == 3020) [
            app
            database
            network
            volume
          ];
          message = "Every Hindsight Quadlet object must run as the rootless service user.";
        }
        {
          assertion = lib.hasInfix "ConditionUser=3020" app.text;
          message = "The generated Hindsight Quadlet must be a UID-gated user unit.";
        }
        {
          assertion = lib.hasInfix "PublishPort=127.0.0.1:8888:8888" app.text;
          message = "The Hindsight API must bind only to loopback.";
        }
        {
          assertion = lib.hasInfix "PublishPort=127.0.0.1:9999:9999" app.text;
          message = "The Hindsight control plane must bind only to loopback.";
        }
        {
          assertion = ingress.forwardAuth == false;
          message = "Hindsight must use its native authentication, not Authentik forward auth.";
        }
        {
          assertion = config.modules.services.ingress.domains == { };
          message = "Hindsight must not be added to an external ingress tunnel.";
        }
        {
          assertion =
            config.services.nginx.virtualHosts."hindsight.socozy.casa".locations."/v1/".proxyPass
            == "http://127.0.0.1:8888";
          message = "The direct Hindsight API route must target the loopback API port.";
        }
        {
          assertion = lib.elem "HINDSIGHT_API_TENANT_EXTENSION=hindsight_api.extensions.builtin.tenant:ApiKeyTenantExtension" app.containerConfig.Environment;
          message = "The Hindsight API-key tenant extension must be enabled.";
        }
        {
          assertion =
            lib.hasInfix "HINDSIGHT_API_TENANT_API_KEY=" appTemplate.content
            && lib.hasInfix "HINDSIGHT_CP_DATAPLANE_API_KEY=" appTemplate.content
            && lib.hasInfix "HINDSIGHT_CP_ACCESS_KEY=" appTemplate.content;
          message = "The Hindsight native-auth secrets must be rendered for both API and control plane.";
        }
        {
          assertion =
            appTemplate.owner == "hindsight"
            && appTemplate.group == "hindsight"
            && appTemplate.mode == "0400"
            && dbTemplate.owner == "hindsight"
            && dbTemplate.group == "hindsight"
            && dbTemplate.mode == "0400";
          message = "Hindsight environment files must be private to the rootless service user.";
        }
        {
          assertion =
            options.modules.services.hindsight.image.default == "ghcr.io/vectorize-io/hindsight:0.8.4";
          message = "The production Hindsight image must remain pinned.";
        }
        {
          assertion =
            options.modules.services.hindsight.postgresImage.default == "docker.io/pgvector/pgvector:pg18";
          message = "The production pgvector image must remain pinned.";
        }
      ];
    };

  testScript = ''
    import json

    user = "hindsight"

    machine.start()
    machine.wait_for_unit("multi-user.target")
    machine.wait_for_unit("default.target", user=user)
    machine.succeed(
        "systemctl show -p Result --value sops-install-secrets.service | grep -Fxq success"
    )
    machine.succeed(
        "test $(stat -c '%U:%G:%a' /run/secrets/rendered/hindsight-app.env) = hindsight:hindsight:400"
    )
    machine.wait_for_unit("hindsight-db.service", user=user, timeout=120)
    machine.wait_for_unit("hindsight.service", user=user, timeout=120)
    machine.wait_for_open_port(8888)
    machine.wait_for_open_port(9999)

    assert "healthy" in machine.succeed("curl -fsS http://127.0.0.1:8888/health")
    assert "hindsight-control-plane" in machine.succeed("curl -fsS http://127.0.0.1:9999/")

    machine.succeed("ss -ltn | grep -q '127.0.0.1:8888'")
    machine.succeed("ss -ltn | grep -q '127.0.0.1:9999'")
    machine.fail("ss -ltn | grep -Eq '0\\.0\\.0\\.0:(8888|9999)'")

    root_containers = json.loads(machine.succeed("podman ps --format json"))
    assert root_containers == [], f"Expected no root-owned containers, got: {root_containers}"

    rootless_containers = json.loads(
        machine.succeed("sudo -u hindsight -- podman ps --format json")
    )
    names = sorted(container["Names"][0] for container in rootless_containers)
    assert names == ["hindsight", "hindsight-db"], f"Unexpected rootless containers: {names}"

    environment = machine.succeed(
        "sudo -u hindsight -- podman inspect hindsight "
        "--format '{{range .Config.Env}}{{println .}}{{end}}'"
    )
    for expected in [
        "HINDSIGHT_API_LLM_PROVIDER=openai",
        "HINDSIGHT_API_LLM_MODEL=gpt-5-mini",
        "HINDSIGHT_API_VECTOR_EXTENSION=pgvector",
        "HINDSIGHT_API_TENANT_EXTENSION=hindsight_api.extensions.builtin.tenant:ApiKeyTenantExtension",
        "HINDSIGHT_API_TENANT_API_KEY=test-api-key",
        "HINDSIGHT_CP_DATAPLANE_API_KEY=test-api-key",
        "HINDSIGHT_CP_ACCESS_KEY=test-control-plane-key",
    ]:
        assert expected in environment, f"Missing Hindsight environment setting: {expected}"

    machine.succeed(
        "sudo -u hindsight -- podman exec hindsight-db "
        "grep -Fxq initialized /var/lib/postgresql/18/docker/hindsight-test"
    )

    volume = json.loads(
        machine.succeed("sudo -u hindsight -- podman volume inspect hindsight-db-data")
    )[0]
    assert volume["Mountpoint"].startswith("/var/lib/hindsight/"), volume["Mountpoint"]
  '';
}
