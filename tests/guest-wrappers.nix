{ pkgs }:
let
  resolve = import ../pkgs/resolve-targets.nix;
  host.config = {
    microvm.vms.guest = { };
    microvm.stateDir = "/var/lib/microvms";
    programs.ssh.knownHosts.microvm-guest.publicKey = "pinned";
  };
  guest.config = {
    networking.hostName = "guest";
    modules.microvm-guest = {
      enable = true;
      host = "host";
    };
    microvm = {
      deploy.sshSwitch = "available";
      hypervisor = "qemu";
      vsock = {
        cid = 4244;
        ssh.enable = true;
      };
    };
  };
  configurations = { inherit host guest; };
  resolved = resolve {
    inherit configurations;
    names = [
      "host"
      "guest"
    ];
  };
  fails = args: !(builtins.tryEval (builtins.deepSeq (resolve args) true)).success;
  guestArgs = {
    inherit configurations;
    names = [ "guest" ];
  };
  badGuest =
    update:
    guestArgs
    // {
      configurations = configurations // {
        guest.config = pkgs.lib.recursiveUpdate guest.config update;
      };
    };
  resolveVsock =
    cs:
    (resolve {
      configurations = cs;
      names = [ "guest" ];
    }).guest;
in
assert
  resolved == {
    host = {
      guest = false;
      buildAttribute = "nixosConfigurations.host.config.system.build.toplevel";
    };
    guest = {
      guest = true;
      buildAttribute = "nixosConfigurations.guest.config.microvm.declaredRunner";
      host = "host";
      guestSSH = "vsock/4244";
      activation = "hot";
    };
  };
assert fails (guestArgs // { names = [ "missing" ]; });
assert fails (badGuest {
  modules.microvm-guest.host = null;
});
assert fails (badGuest {
  modules.microvm-guest.host = "missing";
});
assert fails (badGuest {
  modules.microvm-guest.host = "guest";
});
assert fails (
  guestArgs
  // {
    configurations = configurations // {
      host.config = { };
    };
  }
);
assert fails (badGuest {
  microvm.vsock.ssh.enable = false;
});
assert fails (badGuest {
  microvm.hypervisor = "unsupported";
});
assert fails {
  configurations = configurations // {
    host.config = builtins.removeAttrs host.config [ "programs" ];
  };
  names = [ "guest" ];
};
assert
  (resolveVsock (
    pkgs.lib.recursiveUpdate configurations {
      guest.config.microvm.hypervisor = "cloud-hypervisor";
    }
  )).guestSSH == "vsock-mux//var/lib/microvms/guest/notify.vsock";
assert
  (resolveVsock (
    pkgs.lib.recursiveUpdate configurations {
      guest.config.microvm.deploy.sshSwitch = null;
    }
  )).activation == "restart";
pkgs.runCommand "guest-build-deploy-wrappers"
  {
    nativeBuildInputs = [ pkgs.python3 ];
  }
  ''
    python ${./guest-wrappers.py} ${pkgs.build}/bin/build ${pkgs.deploy}/bin/deploy
    touch "$out"
  ''
