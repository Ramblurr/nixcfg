{ pkgs }:
let
  resolve = import ../pkgs/resolve-targets.nix;
  host.config.microvm.vms.guest = { };
  guest.config = {
    networking.hostName = "guest";
    modules.microvm-guest = {
      enable = true;
      host = "host";
    };
    site.net.svc.hosts4.guest = [ "192.0.2.23" ];
    microvm.deploy.sshSwitch = "available";
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
  vsockConfigurations = pkgs.lib.recursiveUpdate configurations {
    host.config = {
      microvm.stateDir = "/var/lib/microvms";
      programs.ssh.knownHosts.microvm-guest.publicKey = "pinned";
    };
    guest.config.microvm = {
      hypervisor = "qemu";
      vsock = {
        cid = 4244;
        ssh.enable = true;
      };
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
      guestIP = "192.0.2.23";
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
  site.net.svc.hosts4.guest = [ ];
});
assert fails (badGuest {
  site.net.svc.hosts4.guest = [
    "192.0.2.23"
    "192.0.2.24"
  ];
});
assert fails (badGuest {
  microvm.deploy.sshSwitch = null;
});
assert (resolveVsock vsockConfigurations).guestSSH == "vsock/4244";
assert
  (resolveVsock (
    pkgs.lib.recursiveUpdate vsockConfigurations {
      guest.config.microvm.hypervisor = "cloud-hypervisor";
    }
  )).guestSSH == "vsock-mux//var/lib/microvms/guest/notify.vsock";
assert fails {
  configurations = vsockConfigurations // {
    inherit host;
  };
  names = [ "guest" ];
};
assert fails {
  configurations = pkgs.lib.recursiveUpdate vsockConfigurations {
    guest.config.microvm.hypervisor = "unsupported";
  };
  names = [ "guest" ];
};
pkgs.runCommand "guest-build-deploy-wrappers"
  {
    nativeBuildInputs = [ pkgs.python3 ];
  }
  ''
    python ${./guest-wrappers.py} ${pkgs.build}/bin/build ${pkgs.deploy}/bin/deploy
    touch "$out"
  ''
