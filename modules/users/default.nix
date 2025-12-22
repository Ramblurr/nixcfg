{
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
  withImpermanence = config.modules.impermanence.enable;
in
{
  options = {
    modules.users = {
      enable = lib.mkEnableOption "";
      userborn.enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
      };
      headless.enable = lib.mkEnableOption "";
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
        password.enable = lib.mkOption {
          type = lib.types.bool;
          default = true;
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
    services.userborn.enable = cfg.userborn.enable;
    services.pcscd.enable = true;
    systemd.services.sops-install-secrets = {
      after = [ "pcscd.service" ];
      requires = [ "pcscd.service" ];
    };
    sops = {
      age = {
        sshKeyPaths = [
          "${lib.optionalString withImpermanence "/persist"}/etc/ssh/ssh_host_ed25519_key"
        ];
        plugins = [
          pkgs.age-plugin-yubikey
        ];
      };
    };

    sops.secrets."${cfg.primaryUser.passwordSecretKey}" = lib.mkIf cfg.primaryUser.password.enable {
      neededForUsers = true;
    };
    sops.secrets.root-password = {
      neededForUsers = true;
    };
    environment.persistence."/persist" = lib.mkIf withImpermanence {
      users.root.home = "/root";
      users.${cfg.primaryUser.username} = {
        directories = [
          ".config/sops"
        ];
      };
    };
    systemd.tmpfiles.rules =
      let
        u = cfg.primaryUser.username;
      in
      [
        "d ${lib.optionalString withImpermanence "/persist"}/home/${u}/.config/sops/age 0700 ${u} ${u}  -"
      ];
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
        hashedPasswordFile =
          if cfg.primaryUser.password.enable then
            config.sops.secrets."${cfg.primaryUser.passwordSecretKey}".path
          else
            null;
        inherit (cfg.primaryUser) extraGroups;
        inherit (cfg.primaryUser) uid;
        group = cfg.primaryUser.username;
        inherit (cfg.primaryUser) shell;
      };
    };
    # see https://github.com/nikstur/userborn/issues/7
    environment.etc =
      let
        # fixed mappings to preserve
        fixedSubs = {
          invoiceninja2 = {
            start = 362144;
            count = 65536;
          };
        };
        subsLine =
          name: u:
          if (u.isNormalUser or false) && (u.uid != null) then
            let
              # Use fixed start/count if present, otherwise derive from UID.
              start = lib.attrByPath [ name "start" ] (100000 + u.uid * 65536) fixedSubs;
              count = lib.attrByPath [ name "count" ] 65536 fixedSubs;
            in
            "${name}:${toString start}:${toString count}\n"
          else
            "";

        autosubs = lib.concatStrings (lib.mapAttrsToList subsLine config.users.users);
      in
      {
        "subuid".text = autosubs;
        "subuid".mode = "0444";
        "subgid".text = autosubs;
        "subgid".mode = "0444";
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
          inherit (config.sops) defaultSopsFile;
          #gnupg.home = hm.config.programs.gpg.homedir;
          environment = {
            PINENTRY_PROGRAM =
              if cfg.headless.enable then
                "${pkgs.pinentry-tty}/bin/pinentry"
              else
                "${pkgs.pinentry-gtk2}/bin/pinentry";
            PATH = lib.mkForce (
              lib.makeBinPath (
                hm.config.sops.age.plugins ++ lib.optionals (!cfg.headless.enable) [ pkgs.pinentry-gtk2 ]
              )
            );
          };
          age.keyFile = "${hm.config.home.homeDirectory}/.config/sops/age/keys.txt";
          age.plugins = [
            pkgs.age-plugin-fido2-hmac
            pkgs.age-plugin-yubikey
            pkgs.age-plugin-tpm
          ];
        };
        home.packages = lib.optionals (!cfg.headless.enable) [ pkgs.pinentry-gtk2 ];
        manual.manpages.enable = lib.mkDefault (!cfg.headless.enable);
        systemd.user.startServices = true;
        programs.home-manager.enable = true;
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
