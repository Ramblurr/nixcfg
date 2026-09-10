# Resolve only configurations exported by the calling (private) flake.
{
  configurations,
  names,
}:
builtins.listToAttrs (
  map (
    name:
    let
      fail = reason: throw "${name}: ${reason}";
      inherit ((configurations.${name} or (fail "not exported by this flake; run from nixcfg-private")))
        config
        ;
      guest = config.modules.microvm-guest.enable or false;
      host = config.modules.microvm-guest.host or null;
      addresses = config.site.net.svc.hosts4.${config.networking.hostName} or [ ];
      hostConfig = (configurations.${host} or (fail "guest host is not exported by this flake")).config;
      vsock = config.microvm.vsock.ssh.enable or false;
      vsockDeployment =
        if !vsock then { }
        else if !(builtins.hasAttr "microvm-${name}" (hostConfig.programs.ssh.knownHosts or { })) then
          fail "VSOCK deployment requires host-owned SSH configuration and a pinned guest key"
        else {
          guestSSH =
            if config.microvm.hypervisor == "qemu" then
              "vsock/${toString config.microvm.vsock.cid}"
            else if config.microvm.hypervisor == "cloud-hypervisor" then
              "vsock-mux/${hostConfig.microvm.stateDir}/${name}/notify.vsock"
            else fail "unsupported VSOCK deployment hypervisor";
        };
      deployment =
        if !guest then
          { }
        else if host == null || host == "" then
          fail "guest has no host"
        else if hostConfig.modules.microvm-guest.enable or false then
          fail "guest host is itself a guest"
        else if !(builtins.hasAttr name (hostConfig.microvm.vms or { })) then
          fail "guest is not registered on ${host}"
        else if builtins.length addresses != 1 then
          fail "guest needs exactly one service-network IPv4 address"
        else if (config.microvm.deploy.sshSwitch or null) == null then
          fail "guest does not support SSH switching; refusing an implicit VM restart"
        else
          {
            inherit host;
            guestIP = builtins.head addresses;
          } // vsockDeployment;
    in
    {
      inherit name;
      value = {
        inherit guest;
        buildAttribute =
          "nixosConfigurations.${name}.config."
          + (if guest then "microvm.declaredRunner" else "system.build.toplevel");
      }
      // deployment;
    }
  ) names
)
