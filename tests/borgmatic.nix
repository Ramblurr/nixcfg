{ inputs, pkgs }:
let
  lib = inputs.nixpkgs.lib;
  groups = import ../modules/site/gatus-groups.nix;
  secretFile = pkgs.writeText "borgmatic-test-secrets.yaml" "{}\n";
  hostName = "quine";
  gatusUrl = "https://status.example.test";
  heartbeat = lib.getExe (pkgs.callPackage ../pkgs/gatus-heartbeat.nix { });
  startFile = "/run/borgmatic/gatus-start";
  evaluate =
    enable:
    lib.nixosSystem {
      modules = [
        inputs.sops-nix.nixosModules.sops
        ../modules/services/onepassword-systemd-credentials.nix
        ../modules/site/gatus.nix
        ../modules/site/gatus-heartbeats.nix
        ../modules/site/gatus-heartbeats-onepassword.nix
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
        ''${heartbeat} report --url "${gatusUrl}" --group "${groups.infrastructure}" --name "Borgmatic Backup (${hostName})" --success true --duration "$(( $(${date} +%s) - $(cat ${startFile}) ))s"''
      ];
    }
    {
      after = "error";
      when = [ "create" ];
      run = [
        ''${heartbeat} report --url "${gatusUrl}" --group "${groups.infrastructure}" --name "Borgmatic Backup (${hostName})" --success false --error "{error}"''
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
      group = groups.infrastructure;
      token = "$GATUS_EXTERNAL_TOKEN";
      heartbeat.interval = "30h";
      alerts = [ { type = "pushover"; } ];
    }
  ];
assert disabled.site.gatus.externalEndpoints == [ ];
pkgs.runCommand "borgmatic-gatus-test" { } ''
  touch "$out"
''
