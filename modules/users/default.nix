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
      primaryUser = {
        username = lib.mkOption {
          type = lib.types.uniq lib.types.str;
        };
        name = lib.mkOption {
          type = lib.types.uniq lib.types.str;
        };
        email = lib.mkOption {
          type = lib.types.uniq lib.types.str;
        };
        signingKey = lib.mkOption {
          type = lib.types.uniq lib.types.str;
        };
        homeDirectory = lib.mkOption {
          type = lib.types.uniq lib.types.str;
        };

        uid = lib.mkOption {
          default = 1000;
          type = lib.types.uniq lib.types.int;
        };
        passwordSecretKey = lib.mkOption {
          type = lib.types.uniq lib.types.str;
        };
        shell = lib.mkOption {
          type = types.nullOr (types.either types.shellPackage (types.passwdEntry types.path));
          default = pkgs.zsh;
        };
        extraGroups = lib.mkOption {
          type = types.listOf types.str;
          description = lib.mdDoc "The user's auxiliary groups.";
        };
        authorizedKeys = lib.mkOption {
          type = types.listOf types.str;
          description = lib.mdDoc "The user's authorized SSH keys.";
        };
      };
    };
  };
  imports = [
    (mkAliasOptionModule
      [ "myhm" ]
      [
        "home-manager"
        "users"
        cfg.primaryUser.username
      ]
    )
  ];
  config = lib.mkIf cfg.enable {
    sops = {
      age.sshKeyPaths = [
        "${lib.optionalString withImpermanence "/persist"}/etc/ssh/ssh_host_ed25519_key"
      ];
      gnupg.sshKeyPaths = [ ];
    };

    sops.secrets."${cfg.primaryUser.passwordSecretKey}" = {
      neededForUsers = true;
    };
    sops.secrets.root-password = {
      neededForUsers = true;
    };
    environment.persistence = lib.mkIf withImpermanence {
      "/persist".users.root.home = "/root";
    };
    users = {
      mutableUsers = false;
      users.root = {
        initialHashedPassword = lib.mkForce null;
        hashedPasswordFile = config.sops.secrets.root-password.path;
        openssh.authorizedKeys.keys = config.repo.secrets.global.pubKeys;
        shell = pkgs.zsh;
      };
      groups."${cfg.primaryUser.username}".gid = cfg.primaryUser.uid;
      users."${cfg.primaryUser.username}" = {
        isNormalUser = true;
        home = cfg.primaryUser.homeDirectory;
        description = cfg.primaryUser.name;
        openssh.authorizedKeys.keys = cfg.primaryUser.authorizedKeys;
        hashedPasswordFile = config.sops.secrets."${cfg.primaryUser.passwordSecretKey}".path;
        extraGroups = cfg.primaryUser.extraGroups;
        uid = cfg.primaryUser.uid;
        group = cfg.primaryUser.username;
        shell = cfg.primaryUser.shell;
      };
    };

    home-manager = {
      useGlobalPkgs = true;
      backupFileExtension = "backup";
      useUserPackages = true;
      verbose = true;
      extraSpecialArgs = {
        inherit inputs;
      };
      sharedModules = [
        {
          home.stateVersion = lib.mkDefault config.system.stateVersion;
        }
      ];
    };
    # This is an alias for
    # home-manager.users."${username}" = ...
    myhm =
      { pkgs, ... }@hm:
      {
        imports = [
          inputs.impermanence.nixosModules.home-manager.impermanence
          inputs.sops-nix.homeManagerModule
          (mkAliasOptionModule
            [ "persistence" ]
            [
              "home"
              "persistence"
              "/persist${cfg.primaryUser.homeDirectory}"
            ]
          )
        ];
        home.homeDirectory = cfg.primaryUser.homeDirectory;
        sops = {
          defaultSopsFile = config.sops.defaultSopsFile;
          gnupg.home = "${hm.config.xdg.configHome}/.gnupg";
        };
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
        # This is an alias for
        #home.persistence."/persist/home/${cfg.primaryUser.username}" = ..
        persistence = lib.mkIf config.modules.impermanence.enable { allowOther = true; };
      };
  };
}
