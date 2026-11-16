{
  coreutils,
  efibootmgr,
  gnugrep,
  gnused,
  writeShellApplication,
}:
writeShellApplication {
  name = "ludwig-boot";
  runtimeInputs = [
    coreutils
    efibootmgr
    gnugrep
    gnused
  ];
  runtimeEnv.LUDWIG_TRUENAS_ESP_UUID = (import ./site.nix).truenasEspUuid;
  text = builtins.readFile ./ludwig-boot.sh;
}
