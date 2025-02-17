{
  config,
  lib,
  pkgs,
  ...
}:
{
  sops.secrets.root-password = {
    neededForUsers = true;
  };
  users.users.root = {
    initialHashedPassword = lib.mkForce null;
    hashedPasswordFile = config.sops.secrets.root-password.path;
    openssh.authorizedKeys.keys = config.repo.secrets.global.pubKeys;
    shell = pkgs.zsh;
  };

  # This cannot currently be derived automatically due to a design flaw in nixpkgs.
  environment.persistence."/persist".users.root.home = "/root";

  home-manager.users.root = {
    home = {
      username = config.users.users.root.name;
    };
  };
}
