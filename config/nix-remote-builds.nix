{
  config,
  lib,
  pkgs,
  ...
}:
# Keep the client -> builder graph acyclic. A builder that is also a client
# can forward received builds to its own builders, and a cycle can deadlock on
# output locks held by both ends.
let
  inherit (config.networking) hostName;
  prim = config.site.net.prim.hosts4;
  data = config.site.net.data.hosts4;

  builders = {
    quine = {
      address = builtins.head data.quine;
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFICZ13T85//UvjEjf+I72FqaXyGJNt9LD4mjYSq3LTl";
      speedFactor = 2;
      maxJobs = 4;
      supportedFeatures = [
        "nixos-test"
        "benchmark"
        "big-parallel"
        "kvm"
      ];
    };
    peirce = {
      address = builtins.head prim.peirce;
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILa8ve0f0zLpO1AI7opNBEirdQucHM0HsXGpR+f1pA48";
      speedFactor = 1.5;
      maxJobs = 2;
      supportedFeatures = [
        "nixos-test"
        "benchmark"
        "big-parallel"
        "kvm"
      ];
    };
    thingstead = {
      address = builtins.head data.thingstead;
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICHrsPyW6qZoi6QEMyYL1oeN7Us3VNQKmR96uDcAlA08";
      speedFactor = 1;
      maxJobs = 4;
      supportedFeatures = [
        "nixos-test"
        "benchmark"
        "big-parallel"
        "kvm"
      ];
    };
  };

  clients = {
    mootforge = {
      sshKey = "/var/lib/nixbot/.ssh/id_ed25519";
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBRYhwBrSDvAH+qmr3KmMOjiVJpNgUpy4WFBserUtLiz nixbot@mootforge";
      builders = [
        "quine"
        "peirce"
        "thingstead"
      ];
    };
  };

  order = lib.toposort (a: b: lib.elem b (clients.${a}.builders or [ ])) (
    lib.attrNames (builders // clients)
  );
  selfLoops = lib.filter (n: lib.elem n clients.${n}.builders) (lib.attrNames clients);
  client = clients.${hostName} or null;
  inboundKeys = lib.mapAttrsToList (_: c: c.publicKey) (
    lib.filterAttrs (_: c: lib.elem hostName c.builders) clients
  );
in
{
  config = lib.mkMerge [
    {
      assertions = [
        {
          assertion = order ? result && selfLoops == [ ];
          message = "nix-remote-builds: the client -> builder graph has a cycle: ${
            toString (order.cycle or selfLoops)
          }";
        }
      ];
    }

    (lib.mkIf (client != null) {
      nix.distributedBuilds = true;
      nix.buildMachines = map (name: {
        hostName = builders.${name}.address;
        system = "x86_64-linux";
        protocol = "ssh-ng";
        inherit (builders.${name}) maxJobs speedFactor supportedFeatures;
        sshUser = "nix-remote-build";
        inherit (client) sshKey;
      }) client.builders;
      programs.ssh.extraConfig = lib.concatMapStrings (name: ''
        Host ${builders.${name}.address}
          ConnectTimeout 5
      '') client.builders;
      programs.ssh.knownHosts = lib.genAttrs client.builders (name: {
        hostNames = [ builders.${name}.address ];
        publicKey = builders.${name}.hostKey;
      });
    })

    (lib.mkIf (inboundKeys != [ ]) {
      users.users.nix-remote-build = {
        isSystemUser = true;
        group = "nix-remote-build";
        shell = pkgs.bash;
        openssh.authorizedKeys.keys = inboundKeys;
      };
      users.groups.nix-remote-build = { };
      nix.settings.trusted-users = [ "nix-remote-build" ];
    })
  ];
}
