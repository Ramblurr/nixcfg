{ pkgs, ... }:
let
  webRoot = pkgs.runCommand "firewall-probe-root" { } ''
    mkdir -p $out/www
    echo firewall-probe > $out/www/index.html
  '';
  image = pkgs.dockerTools.buildLayeredImage {
    name = "firewall-probe";
    tag = "test";
    contents = [
      pkgs.busybox
      webRoot
    ];
    config.Cmd = [
      "${pkgs.busybox}/bin/httpd"
      "-f"
      "-p"
      "8080"
      "-h"
      "/www"
    ];
  };
in
pkgs.testers.runNixOSTest {
  name = "firewall-runtime-tables";
  nodes = {
    machine =
      { lib, ... }:
      {
        imports = [ ../modules/firewall ];
        options = {
          modules.vpn.mullvad.enable = lib.mkEnableOption "Mullvad";
          modules.vpn.tailscale.enable = lib.mkEnableOption "Tailscale";
        };
        config = {
          system.stateVersion = "23.05";
          modules.firewall.enable = true;
          networking.firewall.allowedTCPPorts = [ 8088 ];
          networking.nftables.tables.obsolete-probe = {
            family = "inet";
            content = "chain marker { }";
          };
          virtualisation.memorySize = 1024;
          virtualisation.podman.enable = true;
          virtualisation.oci-containers = {
            backend = "podman";
            containers.probe = {
              image = "firewall-probe:test";
              imageFile = image;
              ports = [ "127.0.0.1:4005:8080" ];
            };
          };
          environment.systemPackages = [ pkgs.curl ];
          systemd.services = builtins.listToAttrs (
            map
              (port: {
                name = "http-${toString port}";
                value = {
                  wantedBy = [ "multi-user.target" ];
                  serviceConfig.ExecStart = "${pkgs.python3}/bin/python -m http.server ${toString port} --directory ${webRoot}/www";
                };
              })
              [
                8088
                8089
              ]
          );
          specialisation.updated.configuration = {
            networking.firewall.allowedTCPPorts = lib.mkForce [ 8089 ];
            networking.nftables.tables.obsolete-probe.enable = false;
          };
        };
      };
    client.environment.systemPackages = [ pkgs.curl ];
  };
  testScript = ''
    start_all()
    machine.wait_for_unit("podman-probe.service")
    machine.wait_for_open_port(8088)
    machine.wait_for_open_port(8089)
    client.wait_for_unit("multi-user.target")
    published = "curl --fail --max-time 5 http://127.0.0.1:4005/"
    machine.wait_until_succeeds(published)
    container_id = machine.succeed("podman inspect --format '{{.Id}}' probe").strip()
    machine.succeed("nft list table inet obsolete-probe")
    client.succeed("curl --fail --max-time 5 http://machine:8088/")
    client.fail("curl --fail --max-time 3 http://machine:8089/")

    with subtest("reload preserves real Podman port publication on an older stateVersion"):
        machine.succeed("systemctl reload nftables.service")
        machine.succeed(published)
        assert machine.succeed("podman inspect --format '{{.Id}}' probe").strip() == container_id

    with subtest("switch removes obsolete managed rules but preserves runtime publication"):
        machine.succeed("/run/current-system/specialisation/updated/bin/switch-to-configuration test", timeout=120)
        machine.fail("nft list table inet obsolete-probe")
        client.fail("curl --fail --max-time 3 http://machine:8088/")
        client.succeed("curl --fail --max-time 5 http://machine:8089/")
        machine.succeed(published)
        assert machine.succeed("podman inspect --format '{{.Id}}' probe").strip() == container_id
        machine.succeed("systemctl reload nftables.service")
        machine.succeed(published)

    with subtest("legacy deletion state needs one publication repair, then reloads stay safe"):
        machine.succeed("printf 'flush ruleset\\n' > /var/lib/nftables/deletions.nft")
        machine.succeed("systemctl reload nftables.service")
        machine.fail(published)
        machine.succeed("podman network reload probe")
        machine.succeed(published)
        assert machine.succeed("podman inspect --format '{{.Id}}' probe").strip() == container_id
        machine.succeed("systemctl reload nftables.service")
        machine.succeed(published)
        client.fail("curl --fail --max-time 3 http://machine:8088/")
        client.succeed("curl --fail --max-time 5 http://machine:8089/")
  '';
}
