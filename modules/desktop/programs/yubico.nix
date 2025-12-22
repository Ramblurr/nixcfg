{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.yubico;
  perl = "${pkgs.perl}/bin/perl";
  sshAdd = "${pkgs.openssh}/bin/ssh-add";
  pKill = "${pkgs.procps}/bin/pkill";
  awk = "${pkgs.gawk}/bin/awk";

  # fido-add-device is started by a systemd unit. It runs continuously waiting for a USR1 signal
  # that is triggered by inserting a Yubikey. Once it receives the signal, it executes 'ssh-add -K'
  # which when run without a terminal will use SSH_ASKPASS to prompt the user for the unlock
  # phrase for their YK FIDO setup.
  fidoAddDevice = pkgs.writeScriptBin "fido-add-device" ''
    #!${perl}

    use strict;
    use warnings;

    $ENV{'SSH_AUTH_SOCK'} = "$ENV{'XDG_RUNTIME_DIR'}/ssh-agent";
    $ENV{'DISPLAY'} = `systemctl --user show-environment | ${awk} -F= '/^DISPLAY/ {print \$NF}'`;

    $SIG{USR1} = sub { system("${sshAdd}", "-K") };

    while (1) {
      sleep;
    }
  '';

  # fido-send-sig is called by a udev rule when a YK is attached. It sends SIGUSR1 to fido-add-device.
  fidoSendSig = pkgs.writeScriptBin "fido-send-sig" ''
    #! ${pkgs.runtimeShell} -e

    ${pKill} -USR1 -xf "${perl} ${fidoAddDevice}/bin/fido-add-device"
  '';

  # my-ssh-askpass-wrapper wraps programs.ssh.askPassword in order to supply user-specific environment
  # variables.
  # TODO: replace this with makeWrapper
  askPassWrapper = pkgs.writeScript "my-ssh-askpass-wrapper" ''
    #! ${pkgs.runtimeShell} -e
    set -x
    export DISPLAY="$(systemctl --user show-environment | ${awk} -F= '/^DISPLAY/ {print $NF}')"
    export WAYLAND_DISPLAY="$(systemctl --user show-environment | ${awk} -F= '/^WAYLAND_DISPLAY/ {print $NF}')"
    export SSH_AUTH_SOCK="$(echo $XDG_RUNTIME_DIR/ssh-agent)";
    exec ${config.programs.ssh.askPassword} "$@"
  '';
in
{
  options.modules.desktop.programs.yubico = {
    enable = lib.mkEnableOption "";
    sshFidoAgent = {
      enable = lib.mkEnableOption "Add FIDO keys to ssh-agent when attached.";
    };
  };
  config =
    mkIf cfg.enable {
      programs.yubikey-touch-detector.enable = true;
      myhm = {
        services.ssh-agent.enable = true;
        home.packages = with pkgs; [
          yubikey-manager
          yubioath-flutter
          age-plugin-yubikey
          age
          rage
        ];
      };

    }
    // mkIf cfg.sshFidoAgent.enable {
      environment.systemPackages = [ fidoAddDevice ];
      systemd.user.services.ssh-fido-agent = {
        script = ''
          ${fidoAddDevice}/bin/fido-add-device
        '';
        wantedBy = [ "graphical-session.target" ];
        partOf = [ "graphical-session.target" ];
        after = [ "graphical-session.target" ];
        environment.DISPLAY = "fake";
        environment.SSH_ASKPASS = askPassWrapper;
        serviceConfig = {
          Restart = "on-failure";
        };
      };
      services.udev.extraRules = ''
        SUBSYSTEM=="hidraw", ACTION=="add", ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0407|0402", RUN+="${fidoSendSig}/bin/fido-send-sig"
      '';
    };
}
