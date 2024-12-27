{
  options,
  config,
  lib,
  pkgs,
  inputs,
  actual-nixpkgs,
  ...
}:
with lib;
let
  cfg = config.modules.users;
  isEd25519 = k: k.type == "ed25519";
  getKeyPath = k: k.path;
  keys = builtins.filter isEd25519 config.services.openssh.hostKeys;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options = {
    modules.users = {
      enable = lib.mkEnableOption "";
      rootPassword.enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
      };

      mutableUsers = lib.mkOption {
        type = lib.types.bool;
        default = false;
      };
      primaryUser = {
        username = lib.mkOption {
          type = lib.types.uniq lib.types.str;
          default = "ramblurr";
        };
        name = lib.mkOption {
          type = lib.types.uniq lib.types.str;
          default = "Ramblurr";
        };
        email = lib.mkOption {
          type = lib.types.uniq lib.types.str;
          default = "";
        };
        signingKey = lib.mkOption {
          type = lib.types.uniq lib.types.str;
          default = "";
        };
        homeDirectory = lib.mkOption {
          type = lib.types.uniq lib.types.str;
          default = "/home/ramblurr";
        };

        uid = lib.mkOption {
          default = 1000;
          type = lib.types.uniq lib.types.int;
        };

        passwordEnable = lib.mkOption {
          type = lib.types.bool;
          default = true;
        };
        passwordSecretKey = lib.mkOption {
          type = lib.types.uniq lib.types.str;
          default = "ramblurr-password";
        };
        defaultSopsFile = lib.mkOption {
          type = types.nullOr (types.path);
          default = null;
        };
        shell = lib.mkOption {
          type = types.nullOr (types.either types.shellPackage (types.passwdEntry types.path));
          default = pkgs.bash;
        };
        extraGroups = lib.mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = lib.mdDoc "The user's auxiliary groups.";
        };
        authorizedKeys = lib.mkOption {
          type = types.listOf types.str;
          default = config.repo.secrets.global.pubKeys;
          description = lib.mdDoc "The user's authorized SSH keys.";
        };
      };
    };
  };
  imports = [
    (mkAliasOptionModule [ "myhm" ] [
      "home-manager"
      "users"
      cfg.primaryUser.username
    ])
  ];
  config = lib.mkIf cfg.enable {
    sops = {
      age.sshKeyPaths = [
        "${lib.optionalString withImpermanence "/persist"}/etc/ssh/ssh_host_ed25519_key"
      ];
      gnupg.sshKeyPaths = [ ];
      secrets.root-password = lib.mkIf cfg.rootPassword.enable { neededForUsers = true; };
    };

    users = {
      mutableUsers = cfg.mutableUsers;
      users.root.initialHashedPassword = lib.mkForce null;
      users.root.hashedPasswordFile = lib.mkIf cfg.rootPassword.enable config.sops.secrets.root-password.path;

      groups."${cfg.primaryUser.username}".gid = cfg.primaryUser.uid;
      users."${cfg.primaryUser.username}" = {
        isNormalUser = true;
        home = cfg.primaryUser.homeDirectory;
        description = cfg.primaryUser.name;
        openssh.authorizedKeys.keys = cfg.primaryUser.authorizedKeys;
        hashedPasswordFile =
          mkIf cfg.primaryUser.passwordEnable
            config.sops.secrets."${cfg.primaryUser.passwordSecretKey}".path;
        extraGroups = cfg.primaryUser.extraGroups;
        uid = cfg.primaryUser.uid;
        group = cfg.primaryUser.username;
        shell = cfg.primaryUser.shell;
      };
    };

    sops.secrets."${cfg.primaryUser.passwordSecretKey}" = lib.mkIf cfg.primaryUser.passwordEnable {
      #sopsFile = cfg.primaryUser.defaultSopsFile;
      neededForUsers = true;
    };

    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
    };
    # This is an alias for
    # home-manager.users."${username}" = ...
    myhm =
      { pkgs, ... }@hm:
      {
        imports = [
          inputs.impermanence.nixosModules.home-manager.impermanence
          inputs.sops-nix.homeManagerModule
          inputs.ags.homeManagerModules.default
          (mkAliasOptionModule [ "persistence" ] [
            "home"
            "persistence"
            "/persist${cfg.primaryUser.homeDirectory}"
          ])
        ];
        home.stateVersion = "21.11";
        home.homeDirectory = cfg.primaryUser.homeDirectory;
        sops.defaultSopsFile = lib.mkIf (cfg.primaryUser ? defaultSopsFile) cfg.primaryUser.defaultSopsFile;

        manual.manpages.enable = true;
        systemd.user.startServices = true;
        programs = {
          home-manager.enable = true;
        };
        home.extraOutputsToInstall = [
          "info"
          "man"
          "share"
          "icons"
          "doc"
        ];
        home.sessionVariables = {
          EDITOR = "vim";
          CARGO_HOME = "${hm.config.xdg.dataHome}/cargo";
          NIX_PATH = "nixpkgs=flake:nixpkgs\${NIX_PATH:+:$NIX_PATH}";
        };
        nix.registry.nixpkgs.flake = actual-nixpkgs;
        sops = {
          gnupg.home = "${hm.config.xdg.configHome}/.gnupg";
        };
        # This is an alias for
        #home.persistence."/persist/home/${cfg.primaryUser.username}" = ..
        persistence = lib.mkIf config.modules.impermanence.enable { allowOther = true; };
      };
  };
}
