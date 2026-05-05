# this module provides a yubikey fido2  based ssh solution
# this modules _does not work_ with:
# - gpg agent ssh keys in a pgp smartcard applet
# - ssh keys in a piv applet (ala yubikey-agent)
#
# you must have fido2 ssh keys to use this one
{
  config,
  lib,
  pkgs,
  ...
}:
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
in
{
  options.modules.desktop.programs.yubico = {
    enable = lib.mkEnableOption "";
    sshFidoAgent = {
      enable = lib.mkEnableOption "Add FIDO keys to ssh-agent when attached.";
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      environment.systemPackages = with pkgs; [
        yubikey-manager
        yubioath-flutter
        age-plugin-yubikey
        age
        rage
        pinentry-gtk2
        libfido2
      ];
      programs.yubikey-touch-detector.enable = true;
      # FIDO2 OpenSSH keys use the regular ssh-agent socket, not yubikey-agent.
      programs.ssh.startAgent = true;
      programs.ssh.enableAskPassword = false;
      environment.shellInit = ''
        export SSH_AUTH_SOCK=/run/user/$UID/ssh-agent
      '';
      services.udev.packages = with pkgs; [ yubikey-personalization ];
      services.pcscd.enable = true;
      services.gnome.gcr-ssh-agent.enable = false;
      services.yubikey-agent.enable = false;
    })

    (lib.mkIf cfg.sshFidoAgent.enable {
      environment.systemPackages = [ fidoAddDevice ];
      systemd.user.services.ssh-fido-agent = {
        script = ''
          ${fidoAddDevice}/bin/fido-add-device
        '';
        wantedBy = [ "graphical-session.target" ];
        partOf = [ "graphical-session.target" ];
        wants = [ "ssh-agent.service" ];
        after = [
          "graphical-session.target"
          "ssh-agent.service"
        ];
        environment.DISPLAY = "fake";
        #environment.SSH_ASKPASS = askPassWrapper;
        serviceConfig = {
          Restart = "on-failure";
        };
      };
      services.udev.extraRules = ''
        SUBSYSTEM=="hidraw", ACTION=="add", ENV{ID_VENDOR_ID}=="1050", ENV{ID_FIDO_TOKEN}=="1", RUN+="${fidoSendSig}/bin/fido-send-sig"
      '';
    })
  ];
}
