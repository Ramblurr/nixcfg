{
  config,
  pkgs,
  ...
}:
#
# After a sustained time on battery, power off the site router, then this host.
let
  site = import ./site.nix;
  nut = config.power.ups.package;
  passwordFile = config.sops.secrets.upsmonPassword.path;
  shutdown = pkgs.writeShellScript "ludwig-ups-shutdown" ''
    ${pkgs.openssh}/bin/ssh -i ${site.persist.ups}/router_key \
      -o UserKnownHostsFile=${site.persist.ups}/known_hosts \
      -o StrictHostKeyChecking=yes -o BatchMode=yes -o ConnectTimeout=10 \
      root@${site.lan.gateway} ubnt-systool poweroff || true
    ${pkgs.systemd}/bin/shutdown -h now
  '';
  upsschedCmd = pkgs.writeShellScript "ludwig-upssched-cmd" ''
    case "$1" in
      onbatt-timeout) ${nut}/bin/upsmon -c fsd ;;
    esac
  '';
in
{
  # Root-only is enough: the NUT units read it through LoadCredential.
  sops.secrets.upsmonPassword = { };

  power.ups = {
    enable = true;
    mode = "standalone";
    ups.ups = {
      driver = "usbhid-ups";
      port = "auto";
      description = "CyberPower CP1500PFCLCDa";
    };
    users.upsmon = {
      inherit passwordFile;
      upsmon = "primary";
    };
    upsmon = {
      monitor.ups = {
        system = "ups@localhost";
        user = "upsmon";
        inherit passwordFile;
        powerValue = 1;
      };
      settings = {
        MINSUPPLIES = 1;
        RUN_AS_USER = "root";
        SHUTDOWNCMD = "${shutdown}";
        NOTIFYCMD = "${nut}/bin/upssched";
        NOTIFYFLAG =
          map
            (event: [
              event
              "SYSLOG+WALL+EXEC"
            ])
            [
              "ONLINE"
              "ONBATT"
              "LOWBATT"
              "FSD"
              "COMMOK"
              "COMMBAD"
              "SHUTDOWN"
              "REPLBATT"
              "NOCOMM"
            ];
      };
    };
    schedulerRules = toString (
      pkgs.writeText "upssched.conf" ''
        CMDSCRIPT ${upsschedCmd}
        PIPEFN /run/nut/upssched.pipe
        LOCKFN /run/nut/upssched.lock
        AT ONBATT * START-TIMER onbatt-timeout 600
        AT ONLINE * CANCEL-TIMER onbatt-timeout
      ''
    );
  };
}
