{ lib, pkgs, ... }:
{
  # Standalone live installer: never import the installed host's identity,
  # filesystem placeholders, password files, or private SSH host key here.
  networking.hostName = "thinkpad1-installer";
  time.timeZone = "Europe/Vienna";
  i18n.defaultLocale = "de_AT.UTF-8";
  console.keyMap = "de";
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # Local console access is provided by the standard installation-device profile.
  # Remote access must be deliberately configured on the live system.
  services = {
    openssh.settings = {
      PasswordAuthentication = lib.mkForce false;
      KbdInteractiveAuthentication = lib.mkForce false;
    };
    getty.helpLine = lib.mkForce ''
      Live installer only; no target passwords or private host keys are embedded.
      The local nixos/root accounts have empty passwords. SSH password login is disabled.
      For SSH, add your public key to /home/nixos/.ssh/authorized_keys.
      Configure Wi-Fi with nmtui. Do not install the placeholder storage configuration.
    '';
  };
  environment.systemPackages = [
    pkgs.sops
    pkgs.age
    pkgs.ssh-to-age
  ];
  system.stateVersion = "26.05";
}
