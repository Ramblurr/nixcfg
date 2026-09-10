{
  lib,
  writeShellApplication,
  zfs,
  pools,
}:
writeShellApplication {
  name = "zpool-trim-monitored";
  runtimeInputs = [ zfs ];
  text = ''
    result=0
    for pool in ${lib.escapeShellArgs pools}; do
      zpool trim -w "$pool" || result=1
    done
    exit "$result"
  '';
}
