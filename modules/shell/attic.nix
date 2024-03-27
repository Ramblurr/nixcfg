{ options, config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.shell.attic;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.shell.attic = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    systemd.tmpfiles.rules = mkIf withImpermanence [
      "d /persist${homeDirectory}/.config/attic 700 ${username} ${username}"
      "d /persist${homeDirectory}/.local/share/attic 700 ${username} ${username}"
    ];
    home-manager.users."${username}" = { pkgs, config, ... }@hm: {
      home.packages = [ inputs.attic.packages.${pkgs.system}.attic-client ];
      home.persistence."/persist${homeDirectory}" =
        mkIf withImpermanence { directories = [ ".config/attic" ".local/share/attic" ]; };
    };
  };
}
