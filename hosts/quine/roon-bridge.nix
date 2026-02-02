{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.modules.users.primaryUser) username;
  homeDomain = config.repo.secrets.global.domain.home;
in
{
  boot.extraModprobeConfig = ''
    options snd-aloop id=RoonLoopback
  '';
  services.avahi.enable = true;
  environment.persistence."/persist".users.${username}.directories = [
    ".local/state/squeezelite"
  ];

  networking.firewall = {
    allowedTCPPorts = [
      3483 # Squeezebox/squeezelite discovery
    ];
    allowedUDPPorts = [
      3483 # Squeezebox/squeezelite discovery
    ];
  };
  myhm = _: {
    # roon-bridge requires exclusive access to the ALSA device, this makes it
    # not suitable for use on a desktop linux system where pipewire i used to share
    # audio device access among many programs
    # roon supports the old squeezebox api still, so instead of roon-bridge i run
    # squeezelite. however i do not use `services.squeezelite` because it doesn't run as my desktop user.
    systemd.user.services.squeezelite =
      let
        squeezelite = pkgs.squeezelite-pulse;
        squeezelite-bin = "${squeezelite}/bin/${squeezelite.pname}";
        extraArgs = "-s dewey.prim.${homeDomain}";
      in
      {
        Unit = {
          After = [
            "network.target"
            "sound.target"
            "pipewire-pulse.socket"
          ];
          Description = "squeezelite headless player";
          Documentation = "man:squeezelite(5)";
          Wants = [ "sound.target" ];
        };
        Service = {
          ExecStart = "${squeezelite-bin} -N %S/squeezelite/player-name ${extraArgs}";
          Restart = "on-failure";
          RestartMaxDelaySec = 30;
          RestartSteps = 20;
          StateDirectory = "squeezelite";
          Type = "exec";
        };
        Install = {
          WantedBy = [ "default.target" ];
        };
      };

    home.file.".local/share/applications/roon.desktop" = {
      text = ''
        [Desktop Entry]
        Name=Roon
        Exec=flatpak run --command=bottles-cli com.usebottles.bottles run -p Roon -b 'Roon' -- %u
        Type=Application
        Terminal=false
        Categories=Application;
        Icon=/home/ramblurr/.var/app/com.usebottles.bottles/data/bottles/bottles/Roon/icons/Roon.png
        Comment=Launch Roon using Bottles.
        StartupWMClass=Roon
        Actions=Configure;
        [Desktop Action Configure]
        Name=Configure in Bottles
        Exec=flatpak run --command=bottles-cli com.usebottles.bottles -b 'Roon'
      '';
    };
  };
}
