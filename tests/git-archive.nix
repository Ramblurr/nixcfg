{ inputs, pkgs }:
let
  lib = inputs.nixpkgs.lib;
  groups = import ../modules/site/gatus-groups.nix;
  secretFile = pkgs.writeText "git-archive-test-secrets.yaml" "{}\n";
  cfg =
    (lib.nixosSystem {
      modules = [
        inputs.sops-nix.nixosModules.sops
        ../modules/services/onepassword-systemd-credentials.nix
        ../modules/site/gatus.nix
        ../modules/site/gatus-heartbeats.nix
        ../modules/site/gatus-heartbeats-onepassword.nix
        ../modules/services/git-archive.nix
        {
          options = {
            repo.secrets = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
            site.net = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
            modules.zfs.datasets.properties = lib.mkOption {
              type = lib.types.attrsOf (lib.types.attrsOf lib.types.str);
              default = { };
            };
          };
        }
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName = "dewey";
          system.stateVersion = "26.05";
          boot.loader.grub.devices = [ "nodev" ];
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
          sops.defaultSopsFile = secretFile;
          sops.age.keyFile = "/tmp/age-key.txt";
          site.net.mgmt.hosts4.onepassword-connect = [ "192.0.2.22" ];
          repo.secrets.global.domain.home = "example.test";
          modules.services.git-archive.enable = true;
        }
      ];
    }).config;
  credentials = cfg.modules.services.onepassword-systemd-credentials.consumers.gickup;
  service = cfg.systemd.services.gickup;
in
assert
  credentials == {
    gatus-token = "op://home-ops-prod/gatus/borgmatic_external_endpoint_token";
    GITHUB_TOKEN_OL = "op://home-ops-prod/gickup/github-token-ol";
    GITHUB_TOKEN_RAMBLURR = "op://home-ops-prod/gickup/github-token-ramblurr";
  };
assert lib.hasPrefix "!" service.serviceConfig.ExecStopPost;
assert lib.hasInfix "gatus-heartbeat systemd" service.serviceConfig.ExecStopPost;
assert
  cfg.site.gatus.externalEndpoints == [
    {
      name = "Git Archive (dewey)";
      group = groups.work;
      token = "$GATUS_EXTERNAL_TOKEN";
      heartbeat.interval = "30h";
      alerts = [ { type = "pushover"; } ];
    }
  ];
pkgs.runCommand "git-archive-gatus-test" { } ''
  touch "$out"
''
