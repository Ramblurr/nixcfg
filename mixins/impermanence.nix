{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    inputs.impermanence.nixosModules.impermanence
  ];

  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/etc/nixos"
      #"/var/log"
      #"/var/lib"
      #"/srv"
    ];
    files = [
      "/var/lib/dbus/machine-id"
    ];
  };
  programs.fuse.userAllowOther = true;
}
