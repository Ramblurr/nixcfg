{ pkgs }:
let
  runtimeProbe = pkgs.writeText "immich-runtime-probe.cjs" ''
    require('node:assert/strict').equal(process.version, 'v${pkgs.immich-nodejs.version}');
    process.exit(0);
  '';
in
pkgs.runCommand "immich-blob-retention-check"
  {
    nativeBuildInputs = [ pkgs.immich-nodejs ];
    IMMICH_PACKAGE = pkgs.immich;
  }
  ''
    # Exit during Node preload: verify both wrappers without starting Immich.
    for entry in server immich-admin; do
      NODE_OPTIONS="--require ${runtimeProbe}" ${pkgs.immich}/bin/$entry
    done
    node --expose-gc --test ${./immich-blob-retention.cjs}
    touch "$out"
  ''
