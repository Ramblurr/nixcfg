{stdenv}: {
  lib,
  stdenv,
  fetchzip,
  buildFHSUserEnv,
}:
buildFHSUserEnv {
  name = "muse-sounds-manager";
  targetPkgs = pkgs: [pkgs.my.muse-sounds-deb];
  multiPkgs = pkgs: [pkgs.dpkg];
  runScript = "hello";
}
