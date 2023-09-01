{
  pkgs,
  config,
  lib,
  ...
}:
with builtins;
with lib; {
  time.timeZone = lib.mkDefault "Europe/Berlin";
  i18n.defaultLocale = mkDefault "en_US.utf8";
}
