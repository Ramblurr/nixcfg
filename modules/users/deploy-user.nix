{
  config,
  lib,
  pkgs,
  ...
}:

# This module defines some boilerplate for creating "deploy" users
# These are users used for deploying and running applications without root
let
  cfg = config.modules.users.deploy-users;
in
{
  options = {
    modules.users.deploy-users = lib.mkOption {
      default = { };
      type = lib.types.attrsOf (
        lib.types.submodule (
          { config, ... }:
          {
            options = {
              username = lib.mkOption {
                type = lib.types.str;
                description = "The username for the deploy user.";
              };
              homeDirectory = lib.mkOption {
                type = lib.types.path;
                description = "The home directory for the deploy user.";
              };
              homeDirectoryOnZfs = {
                enable = lib.mkOption {
                  type = lib.types.bool;
                  default = false;
                  description = "If true, the home directory will be created as a ZFS dataset.";
                };
                datasetName = lib.mkOption {
                  type = lib.types.str;
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
                description = "The UID for the deploy user.";
              };
              gid = lib.mkOption {
                type = lib.types.nullOr lib.types.int;
                description = "The GID for the deploy user.";
              };
              runtimeDirectory = lib.mkOption {
                type = lib.types.path;
                readOnly = true;
                description = "The runtime directory for the deploy user.";
              };
            };
            config = {
              runtimeDirectory = "/run/user/${toString config.uid}";
            };
          }
        )
      );
    };
  };
  config = {
    users.users = lib.mapAttrs' (
      name: cfg:
      lib.nameValuePair name {
        name = cfg.username;
        isSystemUser = true;
        inherit (cfg) uid;
        shell = pkgs.bash;
        linger = true;
        home = cfg.homeDirectory;
        createHome = true;
        group = cfg.username;
        extraGroups = [ "systemd-journal" ] ++ cfg.extraGroups;
      }
    ) cfg;

    users.groups = lib.mapAttrs' (
      name: cfg:
      lib.nameValuePair name {
        name = cfg.username;
        inherit (cfg) gid;
      }
    ) cfg;

    modules.zfs.datasets.properties = lib.mkMerge (
      lib.mapAttrsToList (
        _name: cfg:
        lib.optionalAttrs cfg.homeDirectoryOnZfs.enable {
          ${cfg.homeDirectoryOnZfs.datasetName}."mountpoint" = cfg.homeDirectory;
        }
      ) cfg
    );

    systemd.tmpfiles.rules = lib.flatten (
      lib.mapAttrsToList (
        _name: cfg:
        let
          inherit (cfg) homeDirectory runtimeDirectory username;
        in
        [
          "d ${homeDirectory} 0750 ${username} ${username}"
          "d ${runtimeDirectory} 0770 ${username} ${username}"
          "d ${homeDirectory}/.run 0750 ${username} ${username} -"
          "d ${homeDirectory}/.config 0770 ${username} ${username} -"
          "d ${homeDirectory}/.config/systemd/user 0750 ${username} ${username}"
          "d ${homeDirectory}/.local 755 ${username} ${username}"
          "d ${homeDirectory}/.local/state 755 ${username} ${username}"
          "d ${homeDirectory}/.local/state/zsh 755 ${username} ${username}"

        ]
      ) cfg
    );

    home-manager.users = lib.mapAttrs' (
      name: cfg:
      lib.nameValuePair name (
        {
          pkgs,
          config,
          osConfig,
          ...
        }:
        let
          uid = toString osConfig.users.users.${name}.uid;
        in
        {
          assertions = [
            {
              message = "The uid must be set.";
              assertion = uid != null && uid != "";
            }
          ];
          home.homeDirectory = cfg.homeDirectory;
          home.sessionVariables = {
            EDITOR = "vim";
            DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/${uid}/bus";
            XDG_RUNTIME_DIR = "/run/user/${uid}";
          };
          systemd.user.startServices = "sd-switch";
          programs.bash = {
            enable = true;
            initExtra = ''
              [[ -f "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh" ]] && source "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
            '';
          };
          home.packages = [
            pkgs.deploy-rs
          ];
        }
      )
    ) cfg;
  };
}
