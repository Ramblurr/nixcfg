{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

{

  #microvm.vms = {
  #  hello-world = {
  #    # (Optional) A set of special arguments to be passed to the MicroVM's NixOS modules.
  #    #specialArgs = {};

  #    # The configuration for the MicroVM.
  #    # Multiple definitions will be merged as expected.
  #    config = {
  #      microvm = {
  #        hypervisor = "cloud-hypervisor";
  #        socket = "control.socket";
  #        vcpu = 2;
  #        mem = 1024;
  #        shares = [
  #          {
  #            proto = "virtiofs";
  #            tag = "ro-store";
  #            source = "/nix/store";
  #            mountPoint = "/nix/.ro-store";
  #          }
  #        ];
  #        #writableStoreOverlay = "/nix/.rw-store";
  #        interfaces = [
  #          {
  #            type = "tap";
  #            id = "vm-hellowrld";
  #            mac = generateMacAddress "hello-world";
  #          }
  #        ];
  #      };

  #      # Actual nixos config for the microvm
  #      system.stateVersion = "24.11";
  #      networking.firewall.allowedTCPPorts = [ 22 ];
  #      services.openssh = {
  #        enable = true;
  #        settings.PermitRootLogin = "yes";
  #        hostKeys = [
  #          {
  #            path = "/etc/ssh/ssh_host_ed25519_key";
  #            type = "ed25519";
  #          }
  #        ];
  #      };
  #      users.users.root = {
  #        openssh.authorizedKeys.keys = config.modules.users.primaryUser.authorizedKeys;
  #      };
  #      systemd.network.enable = true;
  #      networking.useNetworkd = true;
  #      systemd.network.networks."20-lan" = {
  #        matchConfig.Type = "ether";
  #        networkConfig = {
  #          DHCP = "yes";
  #          ##IPForward = "yes";
  #          #DNSSEC = "no";
  #          #Address = [ "10.9.4.201/22" ];
  #          #Gateway = "10.9.4.1";
  #          #DNS = [ "10.9.4.4" ];
  #          #IPv6AcceptRA = true;
  #          #DHCP = "no";
  #        };
  #      };
  #    };
  #  };
  #};
  ##guests =
  ##  let
  ##    mkGuest =
  ##      guestName:
  ##      {
  ##        enableStorageDataset ? false,
  ##        ...
  ##      }:
  ##      {
  ##        autostart = true;
  ##        zfs."/state" = {
  ##          pool = "rpool";
  ##          dataset = "rpool/encrypted/safe/vms/state-${guestName}";
  ##        };
  ##        zfs."/persist" = {
  ##          pool = "rpool";
  ##          dataset = "rpool/encrypted/safe/vms/${guestName}";
  ##        };
  ##        zfs."/storage" = lib.mkIf enableStorageDataset {
  ##          pool = "storage";
  ##          dataset = "safe/guests/${guestName}";
  ##        };
  ##        modules = [
  ##          ./guests/${guestName}.nix
  ##          {
  ##            #node.secretsDir = ./secrets/${guestName};
  ##            networking.nftables.firewall = {
  ##              zones.untrusted.interfaces = [ config.guests.${guestName}.networking.mainLinkName ];
  ##            };
  ##          }
  ##        ];
  ##      };

  ##    mkMicrovm = guestName: opts: {
  ##      ${guestName} = mkGuest guestName opts // {
  ##        backend = "microvm";
  ##        microvm = {
  ##          system = "x86_64-linux";
  ##          macvtap = "lan";
  ##          baseMac = "1c:69:7a:af:95:03"; # TODO move to config
  ##        };
  ##        extraSpecialArgs = {
  ##          #inherit (inputs.self) nodes globals;
  ##          inherit (inputs.self.pkgs.x86_64-linux) lib;
  ##          inherit inputs;
  ##        };
  ##      };
  ##    };
  ##  in
  ##  ({ } // mkMicrovm "hello-world" { enableStorageDataset = true; });

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
            zfs."/state" = {
              pool = "rpool";
              dataset = "rpool/encrypted/safe/vms/state-${guestName}";
            };
            zfs."/persist" = {
              pool = "rpool";
              dataset = "rpool/encrypted/safe/vms/${guestName}";
            };
            zfs."/storage" = lib.mkIf enableStorageDataset {
              pool = "storage";
              dataset = "safe/guests/${guestName}";
            };
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
              macvtap = "lan";
              baseMac = "1c:69:7a:af:95:03"; # TODO move to config
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
