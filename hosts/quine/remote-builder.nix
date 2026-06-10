{
  pkgs,
  ...
}:
{
  # Accept remote nix builds from debord's nixbot service.
  # The nix-remote-build user is trusted by the nix daemon so it can submit
  # builds; its authorized_keys holds only nixbot's SSH key, limiting access
  # to that single client.
  users.users.nix-remote-build = {
    isSystemUser = true;
    group = "nix-remote-build";
    shell = pkgs.bash;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBRYhwBrSDvAH+qmr3KmMOjiVJpNgUpy4WFBserUtLiz nixbot@debord"
    ];
  };
  users.groups.nix-remote-build = { };

  nix.settings.trusted-users = [ "nix-remote-build" ];
}
