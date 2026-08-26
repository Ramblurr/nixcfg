{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      inputs.quadlet-nix2.nixosModules.default
      inputs.sops-nix.nixosModules.sops
      ../modules/site/gatus.nix
      ../modules/services/invoiceninja.nix
      ../modules/services/onepassword-systemd-credentials.nix
      {
        options = {
          modules.services.caddy.routes = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
          modules.zfs.datasets.properties = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
          site.net = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
        };
      }
      {
        nixpkgs.pkgs = pkgs;
        services.mysql.package = pkgs.mariadb_114;
        networking.hostName = "dewey";
        site.net.mgmt.hosts4 = {
          dewey = [ "192.0.2.14" ];
          mali = [ "192.0.2.3" ];
          onepassword-connect = [ "192.0.2.22" ];
        };
        system.stateVersion = "26.05";
        boot.loader.grub.devices = [ "nodev" ];
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
        };
        modules.services.onepassword-systemd-credentials = {
          enable = true;
          connectHost = "http://127.0.0.1:8080";
          bootstrapTokenFile = "/run/test-connect-token";
        };
        modules.services.invoiceninja = {
          enable = true;
          domain = "invoice.example.test";
          ports.http = 8080;
          subnet.hostAddr = "10.89.2.0/24";
          user = {
            name = "invoiceninja2";
            uid = 3015;
          };
          group = {
            name = "invoiceninja2";
            gid = 3015;
          };
        };
      }
    ];
  };
  cfg = evaluated.config;
  setup = cfg.systemd.services.invoiceninja-env-setup;
  containers = cfg.virtualisation.quadlet.containers;
  backupService = cfg.systemd.services.databasus-invoiceninja-role;
  backupScript = backupService.script;
  provider = cfg.modules.services.onepassword-systemd-credentials;
in
assert containers.invoiceninja-redis.autoStart == false;
assert containers.invoiceninja-app.autoStart == false;
assert containers.invoiceninja-scheduler.autoStart == false;
assert containers.invoiceninja-worker.autoStart == false;
assert
  !(builtins.elem "invoiceninja-env-setup.service" (
    containers.invoiceninja-app.unitConfig.Requires or [ ]
  ));
assert setup.wantedBy == [ "multi-user.target" ];
assert builtins.elem "user@3015.service" setup.after;
assert lib.hasInfix "--machine=invoiceninja2@.host --user restart" setup.script;
assert
  provider.consumers.databasus-invoiceninja-role == {
    MARIADB_PASSWORD = "op://home-ops-prod/databasus-invoiceninja/password";
  };
assert
  backupService.serviceConfig.LoadCredential == [
    "MARIADB_PASSWORD:/run/onepassword-credential-provider.sock"
  ];
assert builtins.elem "mysql.service" backupService.requires;
assert builtins.elem "onepassword-credential-provider.socket" backupService.requires;
assert builtins.elem "mysql.service" backupService.after;
assert builtins.elem "onepassword-credential-provider.socket" backupService.after;
assert backupService.serviceConfig.User == "mysql";
assert lib.hasInfix "CREATE USER IF NOT EXISTS 'databasus_invoiceninja'@'192.0.2.3'" backupScript;
assert lib.hasInfix "FROM_BASE64" backupScript;
assert lib.hasInfix "REVOKE ALL PRIVILEGES, GRANT OPTION" backupScript;
assert lib.hasInfix "GRANT SELECT, SHOW VIEW" backupScript;
assert lib.hasInfix "ON invoiceninja.* TO" backupScript;
assert !(lib.hasInfix "GRANT INSERT" backupScript);
assert cfg.services.mysql.settings.mysqld.bind-address == "0.0.0.0";
assert !(builtins.elem 3306 cfg.networking.firewall.allowedTCPPorts);
assert lib.hasInfix ''iifname "mgmt"'' cfg.networking.firewall.extraInputRules;
assert lib.hasInfix "ip saddr 192.0.2.3/32" cfg.networking.firewall.extraInputRules;
assert lib.hasInfix "ip daddr 192.0.2.14" cfg.networking.firewall.extraInputRules;
assert lib.hasInfix "tcp dport 3306 accept" cfg.networking.firewall.extraInputRules;
assert lib.hasInfix ''comment "Databasus Invoice Ninja logical backup"''
  cfg.networking.firewall.extraInputRules;
pkgs.runCommand "invoiceninja-test" { } ''
  touch "$out"
''
