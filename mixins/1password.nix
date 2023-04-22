{
  config,
  lib,
  pkgs,
  ...
}: {
  users.users.ramblurr.packages = with pkgs; [
    _1password-gui
  ];
}
