{
  config,
  lib,
  pkgs,
  ...
}: {
  services.printing.enable = true;
  services.printing.drivers = [pkgs.cups-brother-mfcl2750dw];
  services.avahi.enable = true;
  services.avahi.nssmdns = true;
  # for a WiFi printer
  #services.avahi.openFirewall = true;
}
