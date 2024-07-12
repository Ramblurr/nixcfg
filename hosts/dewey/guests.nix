{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

{
  modules.services.microvm = {
    enable = true;
    guests =
      let
        mkGuest =
          guestName:
          {
            enableStorageDataset ? false,
            ...
          }:
          {
            autostart = true;
            # temporary state that is wiped on reboot
            zfs."/state" = {
              pool = "rpool";
              dataset = "rpool/encrypted/vms/${guestName}";
            };
            # persistent state
            zfs."/persist" = {
              pool = "rpool";
              dataset = "rpool/encrypted/safe/vms/${guestName}";
            };
            #zfs."/storage" = lib.mkIf enableStorageDataset {
            #  pool = "storage";
            #  dataset = "safe/guests/${guestName}";
            #};
            modules = [
              ./guests/${guestName}.nix
              {
                #node.secretsDir = ./secrets/${guestName};
                #networking.nftables.firewall = {
                #  zones.untrusted.interfaces = [
                #    config.modules.services.microvm.guests.${guestName}.networking.mainLinkName
                #  ];
                #};
              }
            ];
          };
        mkMicrovm = guestName: opts: {
          ${guestName} = mkGuest guestName opts // {
            microvm = {
              system = "x86_64-linux";
              macvtap = "brprim4";
              baseMac = "1c:69:7a:00:00:00"; # TODO move to config
            };
            extraSpecialArgs = {
              #inherit (inputs.self) nodes globals;
              #inherit (inputs.self.pkgs.x86_64-linux) lib;
              inherit lib;
              inherit inputs;
            };
          };
        };
      in
      ({ } // mkMicrovm "hello-world" { enableStorageDataset = true; });
  };
}
