{
  config,
  lib,
  pkgs,
  ...
}: {
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  power.ups = {
    # TODO: use the updated service once this pr is merged: https://github.com/NixOS/nixpkgs/pull/213006
    enable = true;
    ups."apc" = {
      driver = "usbhid-ups";
      port = "auto";
      description = "APC UPS";
      directives = ["default.battery.charge.low = 50"];
    };
  };

  sops.secrets.upsd-users = {
    format = "binary";
    sopsFile = ./upsd.sops.users;
  };
  sops.secrets.upsmon-conf = {
    format = "binary";
    sopsFile = ./upsmon.sops.conf;
  };
  environment.etc."nut/upsd.users".source =
    config.sops.secrets.upsd-users.path;
  environment.etc."nut/upsmon.conf".source =
    config.sops.secrets.upsmon-conf.path;
  environment.etc."nut/upsd.conf".text = ''
    LISTEN 127.0.0.1 3493
  '';
  users.users.nut = {
    uid = 84;
    isSystemUser = true;
    home = "/var/lib/nut";
    createHome = true;
    group = "nut";
    description = "UPS monitor user";
  };
  users.groups."nut" = {gid = 84;};

  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/var/lib/nut"
    ];
  };
}
