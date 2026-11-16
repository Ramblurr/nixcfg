{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.repo.secrets.local) ludwig;
in
{
  # Accounts migrated from TrueNAS. Their uids and gids own existing data on
  # tank, so the values must match the TrueNAS database exactly.
  users.groups = lib.mapAttrs (_: gid: { inherit gid; }) ludwig.groups;
  users.users =
    lib.mapAttrs (
      name: account:
      {
        isSystemUser = true;
        inherit (account) uid group;
        extraGroups = account.extraGroups or [ ];
        description = account.description or name;
        home = account.home or "/var/empty";
        openssh.authorizedKeys.keys = account.authorizedKeys or [ ];
      }
      // lib.optionalAttrs (account ? authorizedKeys) {
        # Forced-command keys still need a real shell to run the command.
        shell = pkgs.bashInteractive;
      }
    ) ludwig.users
    // {
      # Extra root keys for unattended remote administration, kept private.
      root.openssh.authorizedKeys.keys = ludwig.agentSshKeys or [ ];
    };

  # Remote borg clients run `borg serve` through their forced command.
  environment.systemPackages = [ pkgs.borgbackup ];
}
