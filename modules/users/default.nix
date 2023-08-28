{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.users;
  isEd25519 = k: k.type == "ed25519";
  getKeyPath = k: k.path;
  keys = builtins.filter isEd25519 config.services.openssh.hostKeys;
in {
  options = {
    modules.users = {
      rootPassword.enable = mkBoolOpt true;
      mutableUsers = mkBoolOpt false;
      primaryUser = {
        username = mkStrOpt "ramblurr";
        name = mkStrOpt "Ramblurr";
        email = mkStrOpt "";
        signingKey = mkStrOpt "";
        homeDirectory = mkStrOpt "/home/ramblurr";
        uid = mkIntOpt 1000;
        passwordEnable = mkBoolOpt true;
        passwordSecretKey = mkStrOpt "ramblurr-password";
        shell = mkOption {
          type = types.nullOr (types.either types.shellPackage (types.passwdEntry types.path));
          default = pkgs.bash;
        };
        extraGroups = mkOption {
          type = types.listOf types.str;
          default = [];
          description = lib.mdDoc "The user's auxiliary groups.";
        };
        authorizedKeys = mkOption {
          type = types.listOf types.str;
          default = [
            "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCzseqIeUgemCgd3/vxkcmJtpVGFS1P3ajBDYaGHHwziIUO/ENkWrEfv/33DvaaY3QQYnSMePRrsHq5ESanwEdjbMBu1quQZZWhyh/M5rQdbfwFoh2BYjCq5hFhaNUl9cjZk3xjQGHVKlTBdFfpuvWtY9wGuh1rf/0hSQauMrxAZsgXVxRhCbY+/+Yjjwm904BrWxXULbrc5yyfpgwHOHhHbpl8NIQIN6OAn3/qcVb7DlGJpLUjfolkdBTY8zGAJxEWecJzjgwwccuWdrzcWliuw0j4fu/MDOonpVQBCY9WcZeKInGHYAKu+eZ/swxAP+9vAR4mc+l/SBYyzCWvM6zG8ebbDK1mkwq2t0G183/0KSxAPJ7OykFD1a/ifb+cXNYJjshCDN+M95A3s6aMEU4VER/9SmQp3YCZvQEDKOBHlqMqlbw0IYAYE/FfU2se+gLI74JizoHBv2OJcduYdV0Ba97fvrb1lYM+tg0VmKUCwCvI9+ZbT2bJH3sM6SE9xt8+3nx6sKzV6h6FlpvDC60Rr2mANsuW3lbqac05Wnmxzk0C8OoJPCqWEmzjyWLJvPq98cG4obJiNlnp7/7xmmhOwyqcy7gDQum1QDwrUJyBKBsJPelJOZJC0pKkerv4LdSZDTSxEVxomstK/WDzmkPK9uUWTEH69VU/bUMuejTNVQ== cardno:000500006944"
            "ecdsa-sha2-nistp384 AAAAE2VjZHNhLXNoYTItbmlzdHAzODQAAAAIbmlzdHAzODQAAABhBGIiONt3u8BdzaI/ndZLXuDnFBXwxbQ7Nr+Yq4BQNBuri49dknMrwjkPIqsUVUFo+hrfqEYnXlBLaKdof1ZpBdwwY2yHUO01EO18fIFkCswAoiHFf7TZO3a1Ekhnmuc1nw== JuiceSSH"
            "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBJ6XVIQ8DU60p0cjBti+kJgd/UqM1tV4M5gFIoR+I0tQ5XmWU65V91uxfeJMBG0Owoweod2q1qKhH3xic9tIHHA= casey@ipad"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF7TTPusOHuyKotZBcwWqIH3B+cRRDQM46+lvATGNAWO disaster recovery 2021"
          ];
          description = lib.mdDoc "The user's authorized SSH keys.";
        };
      };
    };
  };
  imports = [
    (mkAliasOptionModule ["myhm"] ["home-manager" "users" cfg.primaryUser.username])
  ];
  config = {
    sops = {
      age.sshKeyPaths = ["/persist/etc/ssh/ssh_host_ed25519_key"];
      gnupg.sshKeyPaths = [];
      secrets.root-password = mkIf cfg.rootPassword.enable {
        neededForUsers = true;
      };
    };

    users = {
      mutableUsers = cfg.mutableUsers;
      users.root.initialHashedPassword = null;
      users.root.passwordFile = mkIf cfg.rootPassword.enable config.sops.secrets.root-password.path;

      groups."${cfg.primaryUser.username}".gid = cfg.primaryUser.uid;
      users."${cfg.primaryUser.username}" = {
        isNormalUser = true;
        home = cfg.primaryUser.homeDirectory;
        description = cfg.primaryUser.name;
        openssh.authorizedKeys.keys = cfg.primaryUser.authorizedKeys;
        passwordFile = mkIf cfg.primaryUser.passwordEnable config.sops.secrets."${cfg.primaryUser.passwordSecretKey}".path;
        extraGroups = cfg.primaryUser.extraGroups;
        uid = cfg.primaryUser.uid;
        group = cfg.primaryUser.username;
        shell = cfg.primaryUser.shell;
      };
    };

    sops.secrets."${cfg.primaryUser.passwordSecretKey}" = mkIf cfg.primaryUser.passwordEnable {
      sopsFile = ../../secrets/secrets.sops.yaml;
      neededForUsers = true;
    };

    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
    };
    # This is an alias for
    # home-manager.users."${username}" = ...
    myhm = {pkgs, ...} @ hm: {
      imports = [
        inputs.impermanence.nixosModules.home-manager.impermanence
        inputs.sops-nix.homeManagerModule
        (mkAliasOptionModule ["persistence"] ["home" "persistence" "/persist${cfg.primaryUser.homeDirectory}"])
      ];
      home.stateVersion = "21.11";
      home.homeDirectory = cfg.primaryUser.homeDirectory;
      systemd.user.startServices = true;
      programs = {
        home-manager.enable = true;
      };
      home.extraOutputsToInstall = ["info" "man" "share" "icons" "doc"];
      home.sessionVariables = {
        EDITOR = "vim";
        CARGO_HOME = "${hm.config.xdg.dataHome}/cargo";
        NIX_PATH = "nixpkgs=flake:nixpkgs$\{NIX_PATH:+:$NIX_PATH}";
      };
      nix.registry.nixpkgs.flake = inputs.nixpkgs;
      sops = {
        defaultSopsFile = ../../secrets/secrets.sops.yaml;
        gnupg.home = "${hm.config.xdg.configHome}/.gnupg";
      };
      # This is an alias for
      #home.persistence."/persist/home/${cfg.primaryUser.username}" = ..
      persistence = mkIf config.modules.impermanence.enable {
        allowOther = true;
      };
    };
  };
}
