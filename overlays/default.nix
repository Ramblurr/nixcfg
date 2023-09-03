{
  config,
  lib,
  pkgs,
  ...
}: {
  microsocks = pkgs.callPackage ./packages/microsocks {};
  hacompanion = pkgs.callPackage ./packages/hacompanion {};
}
