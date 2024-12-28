{
  config,
  globals,
  pkgs,
  ...
}:
{
  users.users.root = {
    #inherit (globals.root) hashedPassword;
    openssh.authorizedKeys.keys = globals.pubKeys;
    shell = pkgs.zsh;
  };

  # This cannot currently be derived automatically due to a design flaw in nixpkgs.
  environment.persistence."/persist".users.root.home = "/root";

  home-manager.users.root = {
    imports = [
      ../config
    ];

    home = {
      username = config.users.users.root.name;

      packages = with pkgs; [
        vim
        wireguard-tools
      ];
    };
  };
}
