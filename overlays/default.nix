{
  config,
  lib,
  pkgs,
  ...
}: {
  microsocks = pkgs.callPackage ./packages/microsocks {};
}
