{ lib, ... }:
{
  # Only imported by virtualisation.vmVariant, never by the installed host.
  disabledModules = [ ./storage.nix ];
  networking.hostName = lib.mkForce "thinkpad1-vm";
  modules.users.primaryUser.name = lib.mkForce "Viki (VM)";
  modules.vpn.tailscale.enable = lib.mkForce false;
  services.openssh.enable = lib.mkForce false;
  services.timesyncd.enable = lib.mkForce false;

  sops.secrets = lib.mkForce { };
  systemd.services.sops-install-secrets.enable = lib.mkForce false;
  modules.users.root.enable = lib.mkForce false;
  modules.users.primaryUser.password.enable = lib.mkForce false;
  users.users = {
    root.password = lib.mkForce "test";
    viki.password = lib.mkForce "test";
    # Replace this entry as a whole to avoid evaluating its SOPS password path.
    ramblurr = lib.mkForce {
      isNormalUser = true;
      uid = 1001;
      group = "ramblurr";
      extraGroups = [ "wheel" ];
      password = "test";
    };
  };

  virtualisation = {
    memorySize = 8192;
    cores = 4;
    diskSize = 32768;
    graphics = true;
    qemu.options = [ "-display gtk" ];
  };
}
