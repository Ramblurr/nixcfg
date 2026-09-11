{
  config,
  lib,
  nodes,
  ...
}:
let
  domain = config.repo.secrets.home-ops.homeDomain;
  guestAddress = builtins.head config.site.net.svc.hosts4.opencloud-home;
  dataset = "rpool/encrypted/safe/vms/opencloud-home";
  stateDir = "/var/lib/opencloud-home-vm";
in
{
  microvm.vms.opencloud-home.evaluatedConfig = nodes.opencloud-home;

  modules = {
    zfs.datasets = {
      properties.${dataset}.mountpoint = stateDir;
      services.${dataset} = [ "microvm@opencloud-home" ];
    };
    services = {
      onepassword-systemd-credentials.consumers.opencloud-home-env-setup = {
        IDM_ADMIN_PASSWORD = "op://home-ops-prod/opencloud-home/admin-password";
        JWT_SECRET = "op://home-ops-prod/opencloud-home/office-jwt-secret";
      };
      caddy.routes = {
        opencloud-home = {
          publicHost = "data.${domain}";
          upstream = "http://${guestAddress}:9200";
        };
        opencloud-home-office = {
          publicHost = "euro-office.${domain}";
          upstream = "http://${guestAddress}:9201";
          # Root redirects to the installation welcome page, not the editor.
          staticResponses = lib.genAttrs [ "/" "/welcome" "/welcome/*" ] (_: {
            body = "";
            status = 404;
          });
        };
      };
    };
  };
  systemd.tmpfiles.rules = [ "d ${stateDir} 0700 microvm kvm - -" ];
  assertions = [
    {
      assertion = config.modules.services.onepassword-systemd-credentials.enable;
      message = "OpenCloud host credentials require the 1Password systemd provider.";
    }
  ];
  systemd.services = {
    "microvm@opencloud-home" = {
      overrideStrategy = "asDropin";
      requires = [ "opencloud-home-env-setup.service" ];
      after = [
        "opencloud-home-env-setup.service"
        "systemd-tmpfiles-setup.service"
      ];
    };
    opencloud-home-env-setup = {
      before = [ "microvm@opencloud-home.service" ];
      partOf = [ "microvm@opencloud-home.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        User = "microvm";
        RuntimeDirectory = "opencloud-home-env";
        RuntimeDirectoryMode = "0700";
        UMask = "0077";
      };
      script = ''
        set -eu
        admin_password=$(cat "$CREDENTIALS_DIRECTORY/IDM_ADMIN_PASSWORD")
        office_secret=$(cat "$CREDENTIALS_DIRECTORY/JWT_SECRET")
        printf 'IDM_ADMIN_PASSWORD=%s\n' "$admin_password" > "$RUNTIME_DIRECTORY/app.env.new"
        printf 'JWT_SECRET=%s\n' "$office_secret" > "$RUNTIME_DIRECTORY/office.env.new"
        chmod 0400 "$RUNTIME_DIRECTORY/"*.env.new
        mv -f "$RUNTIME_DIRECTORY/app.env.new" "$RUNTIME_DIRECTORY/app.env"
        mv -f "$RUNTIME_DIRECTORY/office.env.new" "$RUNTIME_DIRECTORY/office.env"
      '';
    };
  };
  site.gatus.endpoints = [
    {
      name = "OpenCloud home";
      group = config.site.gatus.groups.home;
      url = "https://data.${domain}/status.php";
    }
    {
      name = "Euro Office home";
      group = config.site.gatus.groups.home;
      url = "https://euro-office.${domain}/hosting/discovery";
    }
  ];
}
