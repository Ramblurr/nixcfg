{ inputs, pkgs }:
let
  lib = inputs.nixpkgs.lib;
  secretFile = pkgs.writeText "borgmatic-test-secrets.yaml" "{}\n";
  hostName = "quine";
  gatusUrl = "https://status.example.test";
  endpointKey = "infrastructure---operations_borgmatic-backup-(quine)";
  startFile = "/run/borgmatic/gatus-start";
  evaluate =
    enable:
    lib.nixosSystem {
      modules = [
        inputs.sops-nix.nixosModules.sops
        ../modules/site/gatus.nix
        ../modules/services/borgmatic.nix
        {
          options.repo.secrets = lib.mkOption { type = lib.types.attrs; };
        }
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName = hostName;
          system.stateVersion = "26.05";
          boot.loader.grub.devices = [ "nodev" ];
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
          sops.defaultSopsFile = secretFile;
          sops.age.keyFile = "/tmp/age-key.txt";
          repo.secrets.global.domain.home = "example.test";
          modules.services.borgmatic = lib.mkIf enable {
            enable = true;
            name = "test-backup";
            repositories = [
              {
                label = "test";
                path = "/backup";
              }
            ];
          };
        }
      ];
    };
  enabled = (evaluate true).config;
  disabled = (evaluate false).config;
  curl = lib.getExe pkgs.curl;
  date = lib.getExe' pkgs.coreutils "date";
  expectedCommands = [
    {
      before = "action";
      when = [ "create" ];
      run = [ "${date} +%s > ${startFile}" ];
    }
    {
      after = "action";
      when = [ "create" ];
      states = [ "finish" ];
      run = [
        ''${curl} --fail --silent --show-error --retry 3 --request POST --header "Authorization: Bearer $BORGMATIC_GATUS_TOKEN" "${gatusUrl}/api/v1/endpoints/${endpointKey}/external?success=true&duration=$(( $(${date} +%s) - $(cat ${startFile}) ))s" || echo "Failed to report Borgmatic success to Gatus" >&2''
      ];
    }
    {
      after = "error";
      when = [ "create" ];
      run = [
        ''${curl} --fail --silent --show-error --retry 3 --get --request POST --header "Authorization: Bearer $BORGMATIC_GATUS_TOKEN" --data-urlencode "success=false" --data-urlencode error={error} "${gatusUrl}/api/v1/endpoints/${endpointKey}/external" || echo "Failed to report Borgmatic failure to Gatus" >&2''
      ];
    }
  ];
in
assert enabled.services.borgmatic.settings.commands == expectedCommands;
assert !(enabled.services.borgmatic.settings ? healthchecks);
assert enabled.systemd.services.borgmatic.serviceConfig.RuntimeDirectory == "borgmatic";
assert
  enabled.site.gatus.externalEndpoints == [
    {
      name = "Borgmatic Backup (${hostName})";
      group = "Infrastructure & Operations";
      token = "$BORGMATIC_GATUS_TOKEN";
      heartbeat.interval = "30h";
      alerts = [ { type = "pushover"; } ];
    }
  ];
assert disabled.site.gatus.externalEndpoints == [ ];
pkgs.runCommand "borgmatic-gatus-test" { } ''
  touch "$out"
''
