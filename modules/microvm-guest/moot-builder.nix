{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.microvm-guest.mootBuilder;
  inherit (config.modules.microvm-guest.mootVm) network;
in
{
  options.modules.microvm-guest.mootBuilder = {
    enable = lib.mkEnableOption ''
      a Moot builder: a nix-daemon that the tailnet node on its host serves
      over ssh-ng to the agent VMs its record lists
    '';

    daemonPort = lib.mkOption {
      type = lib.types.port;
      default = 7700;
      description = "Guest port where each connection from the node gets its own nix-daemon --stdio.";
    };

    maxJobs = lib.mkOption {
      type = lib.types.ints.positive;
      default = lib.max 1 (config.microvm.vcpu / 2);
      defaultText = lib.literalExpression "max 1 (microvm.vcpu / 2)";
      description = "Concurrent builds, shared by every client.";
    };
  };

  config = lib.mkIf cfg.enable {
    modules.motd.units = [ config.systemd.sockets.nix-remote.name ];
    modules.microvm-guest = {
      mootVm = {
        enable = true;
        network.ingressPort = cfg.daemonPort;
      };
      mounts = [ "var/lib" ];
    };

    nix.settings = {
      max-jobs = cfg.maxJobs;
      cores = lib.max 1 (config.microvm.vcpu / cfg.maxJobs);
    };

    # The connection runs as root, so the daemon trusts it. A build user is
    # root-equivalent here, which is why each trust tier has its own builder.
    systemd.sockets.nix-remote = {
      description = "nix-daemon --stdio for the tailnet node";
      wantedBy = [ "sockets.target" ];
      listenStreams = [ "${network.guestAddress}:${toString cfg.daemonPort}" ];
      socketConfig = {
        Accept = true;
        FreeBind = true;
      };
    };
    systemd.services."nix-remote@" = {
      description = "nix-daemon --stdio for one ssh-ng session";
      requires = [ "nix-daemon.socket" ];
      after = [ "nix-daemon.socket" ];
      environment.NIX_REMOTE = "daemon";
      serviceConfig = {
        ExecStart = "${config.nix.package}/bin/nix-daemon --stdio";
        StandardInput = "socket";
        StandardOutput = "socket";
        StandardError = "journal";
      };
    };
  };
}
