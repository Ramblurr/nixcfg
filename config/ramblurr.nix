{
  config,
  lib,
  pkgs,
  ...
}:
{
  modules.users.primaryUser = {
    username = "ramblurr";
    name = "Casey Link";
    homeDirectory = "/home/ramblurr";
    signingKey = "978C4D08058BA26EB97CB51820782DBCACFAACDA";
    email = "unnamedrambler@gmail.com";
    passwordSecretKey = "ramblurr-password";
    authorizedKeys = config.repo.secrets.global.pubKeys;
  };
}
