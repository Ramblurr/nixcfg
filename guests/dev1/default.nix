{
  lib,
  config,
  pkgs,
  ...
}:
{
  system.stateVersion = "25.11";

  microvm.registerWithMachined = true;
  microvm.machineId = "4ffaf888-492b-4add-a66f-a7ea449bfdc2";
  microvm.vsock.ssh.enable = true;
  microvm.vsock.cid = 4242;
  #microvm.hypervisor = "qemu";

  # goal: Credential-less ssh root login from the host machine _only_
  #       No SSH listeners on any TCP sockets, local or otherwise
  users.users.root.hashedPassword = "";
  services.openssh = {
    enable = true;
    startWhenNeeded = true;
    ports = [ 22 ];
    openFirewall = false;
    settings = {
      PermitRootLogin = "yes";
      PasswordAuthentication = lib.mkForce true;
      PermitEmptyPasswords = true;
      UsePAM = false;
    };
  };
  systemd.sockets.sshd = {
    wantedBy = lib.mkForce [ ];
    socketConfig.ListenStream = lib.mkForce [ ];
  };

  modules.microvm-guest = {
    devSandbox.enable = true;
    #bootstrapSops.enable = true;
    #writableStoreOverlay.enable = true;
    #homeManager = {
    #  enable = true;
    #  username = "${username}";
    #  uid = 1000;
    #  gid = 1000;
    #};
  };
  modules.dev.llms.enable = true;
}
