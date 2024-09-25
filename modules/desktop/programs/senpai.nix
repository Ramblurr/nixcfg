{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.senpai;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.senpai = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {

    systemd.tmpfiles.rules = mkIf withImpermanence [
      "d '/persist${homeDirectory}/.config/senpai' - ${username} ${username} - -"
    ];

    #environment.persistence."/persist" = mkIf withImpermanence {
    #  users.${username} = {
    #    directories = [
    #      ".config/senpai"
    #    ];
    #  };
    #};
    home-manager.users."${username}" =
      { pkgs, config, ... }@hm:
      {
        programs.senpai = {
          enable = true;
          config = {
            address = "irc+insecure://irc.socozy.casa:6667";
            nickname = "ramblurr";
            password-cmd = [
              "op"
              "item"
              "get"
              "--vault"
              "private"
              "senpai-irc"
              "--fields"
              "password"
            ];
          };
        };
      };
  };
}
