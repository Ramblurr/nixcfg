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
  # These uids and gids own existing data on tank.
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
      root.openssh.authorizedKeys.keys = ludwig.agentSshKeys or [ ];
    };

  environment.systemPackages = [ pkgs.borgbackup ];
}
