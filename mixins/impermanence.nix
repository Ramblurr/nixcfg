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
    directories = [
      "/etc/nixos"
      #"/var/log"
      #"/var/lib"
      #"/srv"
    ];
    files = [
      "/etc/machine-id"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
    ];
  };
  programs.fuse.userAllowOther = true;
}
