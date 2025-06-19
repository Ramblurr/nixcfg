{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.users.deploy-users;
in
{
  options = {
    modules.users.deploy-users = lib.mkOption {
      default = { };
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            username = lib.mkOption {
              type = lib.types.str;
              description = "The username for the deploy user.";
            };
            homeDirectory = lib.mkOption {
              type = lib.types.path;
              default = "/var/lib/${cfg.username}";
              description = "The home directory for the deploy user, defaults to /var/lib/<username>.";
            };
            homeDirectoryOnZfs = {
              enable = lib.mkOption {
                type = lib.types.bool;
                default = true;
                description = "If true, the home directory will be created as a ZFS dataset.";
              };
              datasetName = lib.mkOption {
                type = lib.types.str;
                default = "rpool/encrypted/safe/svc/${cfg.username}";
                description = "The name of the ZFS dataset for the home directory.";
              };
            };
            shell = lib.mkOption {
              type = lib.types.shellPackage;
              default = pkgs.bash;
              description = "The shell for the deploy user.";
            };
            extraGroups = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              description = "Extra groups for the deploy user.";
            };
            uid = lib.mkOption {
              type = lib.types.nullOr lib.types.int;
              default = null;
              description = "The UID for the deploy user.";
            };
            gid = lib.mkOption {
              type = lib.types.nullOr lib.types.int;
              default = null;
              description = "The GID for the deploy user.";
            };
          };
        }
      );
    };
  };
  config = {

    users.users = lib.mapAttrs' (
      name: cfg:
      lib.nameValuePair name ({
        name = cfg.username;
        isSystemUser = true;
        shell = pkgs.bash;
        linger = true;
        home = cfg.homeDirectory;
        createHome = true;
        group = cfg.username;
      })
    ) cfg;

    users.groups = lib.mapAttrs' (
      name: cfg:
      lib.nameValuePair name ({
        name = cfg.username;
      })
    ) cfg;
    modules.zfs.datasets.properties = lib.mapAttrs' (
      name: cfg:
      lib.nameValuePair name (
        lib.optionalAttrs cfg.homeDirectoryOnZfs.enable {
          ${cfg.homeDirectoryOnZfs.datasetName}."mountpoint" = cfg.homeDir;
        }
      )
    ) cfg;

    systemd.tmpfiles.rules = lib.flatten (
      lib.mapAttrsToList (
        name: cfg:
        let
          inherit (cfg) homeDirectory username;
        in
        [
          "d ${homeDirectory} 700 ${username} ${username}"
          "d ${homeDirectory}/.config 0775 ${username} ${username} -"
          "d ${homeDirectory}/.local 755 ${username} ${username}"
          "d ${homeDirectory}/.local/state 755 ${username} ${username}"
          "d ${homeDirectory}/.local/state/zsh 755 ${username} ${username}"
        ]
      ) cfg
    );

    home-manager.users = lib.mapAttrs' (
      name: cfg:
      lib.nameValuePair name (
        { pkgs, config, ... }:
        {
          home.homeDirectory = cfg.homeDirectory;
          home.sessionVariables = {
            EDITOR = "vim";
            DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/${toString cfg.uid}/bus";
            XDG_RUNTIME_DIR = "/run/user/${toString cfg.uid}";
          };

          systemd.user.startServices = "sd-switch";
          programs.bash = {
            enable = true;
            initExtra = ''
              [[ -f "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh" ]] && source "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
            '';
          };
        }
      )
    ) cfg;
  };
}
