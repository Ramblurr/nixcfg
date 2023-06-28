# source: https://github.com/knopki/devops-at-home/blob/307921320d6147347e830d2c709f142b809d55b4/nixos/hosts/alien/flatpak.nix#L5
{
  pkgs,
  lib,
  config,
  ...
}: let
  inherit (lib) concatStringsSep stringAfter;
  inherit (lib.generators) toINI;
  inherit (pkgs) writeTextDir symlinkJoin;
  flathub_apps = [
    #"cc.arduino.arduinoide"
    #"com.github.micahflee.torbrowser-launcher"
    #"com.github.tchx84.Flatseal"
    #"fr.handbrake.ghb"
    #"net.poedit.Poedit"
    #"org.audacityteam.Audacity"
    #"org.blender.Blender"
    #"org.fedoraproject.MediaWriter"
    #"org.kde.kdenlive"
    #"org.fritzing.Fritzing"
    #"org.gnome.meld"
    #"org.jitsi.jitsi-meet"
    #"org.kde.krita"
    #"org.libreoffice.LibreOffice"
    #"org.musescore.MuseScore"
    #"org.musicbrainz.Picard"
    #"org.remmina.Remmina"
    #"org.videolan.VLC"
    #"org.zealdocs.Zeal"
    #"rest.insomnia.Insomnia"
    #"us.zoom.Zoom"
  ];
in {
  config = {
    services.flatpak.enable = true;

    environment.persistence."/persist" = {
      directories = [
        "/var/lib/flatpak"
      ];
    };
    home-manager.users.ramblurr = {pkgs, ...}: {
      home.persistence."/persist/home/ramblurr" = {
        directories = [
          ".cache/flatpak"
          ".local/share/flatpak"
          ".var/app"
        ];
      };
    };
    #systemd = {
    #  services.flatpak-setup = {
    #    description = "Setup system Flatpak";
    #    after = ["network-online.target"];
    #    wants = ["network-online.target"];
    #    # this can slow down boot considerably
    #    # wantedBy = ["graphical.target"];
    #    serviceConfig = {
    #      Type = "oneshot";
    #      RemainAfterExit = "yes";
    #    };
    #    script = let
    #      flathub_cmd =
    #        concatStringsSep "\n"
    #        (map
    #          (x: "${pkgs.flatpak}/bin/flatpak install flathub ${x} -y --noninteractive --verbose >> ~/.cache/flatpak.log 2>&1")
    #          flathub_apps);
    #    in ''
    #      ${pkgs.flatpak}/bin/flatpak config --system --set languages "en"
    #      ${pkgs.flatpak}/bin/flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
    #      ${flathub_cmd}
    #      ${pkgs.flatpak}/bin/flatpak uninstall --system --unused -y --noninteractive
    #    '';
    #  };
    #};
  };
}
