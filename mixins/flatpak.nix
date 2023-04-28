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
    #"re.sonny.Junction"
    "cc.arduino.arduinoide"
    "com.calibre_ebook.calibre"
    "com.github.jeromerobert.pdfarranger"
    "com.github.micahflee.torbrowser-launcher"
    "com.github.tchx84.Flatseal"
    "com.logseq.Logseq"
    "com.nextcloud.desktopclient.nextcloud"
    "com.slack.Slack"
    "fr.handbrake.ghb"
    "io.github.seadve.Kooha"
    "net.poedit.Poedit"
    "org.audacityteam.Audacity"
    "org.blender.Blender"
    "org.fedoraproject.MediaWriter"
    "org.kde.kdenlive"
    "org.fritzing.Fritzing"
    "org.gimp.GIMP"
    "org.gnome.meld"
    "org.inkscape.Inkscape"
    "org.jitsi.jitsi-meet"
    "org.kde.kid3"
    "org.kde.krita"
    "org.libreoffice.LibreOffice"
    "org.musescore.MuseScore"
    "org.musicbrainz.Picard"
    "org.remmina.Remmina"
    "org.signal.Signal"
    "org.videolan.VLC"
    "org.zealdocs.Zeal"
    "rest.insomnia.Insomnia"
    "us.zoom.Zoom"
  ];
  flatpak_overrides = map (x: writeTextDir x.name x.text) [
    {
      name = "global";
      text = toINI {} {
        Context = {
          sockets = concatStringsSep ";" [
            "!x11"
            "!fallback-x11"
            "!wayland"
            "!cups"
            "!gpg-agent"
            "!pcsc"
            "!ssh-auth"
            "!session-bus"
            "!system-bus"
            ""
          ];
          shared = "!ipc;!network;";
          features = "!bluetooth;!devel;!multiarch;!canbus;!per-app-dev-shm;";
          devices = "!dri;!kvm;!shm;!all;";
          filesystems = concatStringsSep ";" [
            "!host"
            "!host-etc"
            "!host-os"
            "!home"
            "!xdg-cache"
            "!xdg-config"
            "!xdg-data"
            "!xdg-desktop"
            "!xdg-documents"
            "!xdg-download"
            "!xdg-music"
            "!xdg-pictures"
            "!xdg-public-share"
            "!xdg-templates"
            "!xdg-videos"
            "!xdg-run/keyring"
            "!/media"
            "!/run/media"
            "!/tmp"
            "xdg-config/Kvantum:ro"
            "xdg-config/kdeglobals:ro"
            "xdg-config/gtk-3.0:ro"
            "xdg-config/gtk-4.0:ro"
            ""
          ];
        };
        "System Bus Policy" = {
          "org.freedesktop.Accounts" = "none";
          "org.freedesktop.systemd1" = "none";
          "org.freedesktop.secrets" = "none";
          "org.freedesktop.impl.portal.PermissionStore" = "none";
        };
        "Session Bus Policy" = {
          "org.freedesktop.Flatpak" = "none";
        };
      };
    }

    {
      name = "re.sonny.Junction";
      text = toINI {} {
        Context = {
          sockets = "x11;wayland;fallback-x11;";
          shared = "ipc;";
          devices = "dri;";
          filesystems = concatStringsSep ";" [
            "host:ro"
            "host-etc:ro"
            "host-os:ro"
            "home:ro"
            "xdg-cache:ro"
            "xdg-config:ro"
            "xdg-data:ro"
            "xdg-desktop:ro"
            "xdg-documents:ro"
            "xdg-download:ro"
            "xdg-music:ro"
            "xdg-pictures:ro"
            "xdg-public-share:ro"
            "xdg-templates:ro"
            "xdg-videos:ro"
            "xdg-run/keyring:ro"
            "/media:ro"
            "/run/media:ro"
            "/tmp:ro"
            "/var/lib/flatpak:ro"
            "/var/lib/snapd/desktop:ro"
            "/run/current-system/sw/share:ro"
          ];
        };
        "Session Bus Policy" = {
          "org.freedesktop.Flatpak" = "talk";
        };
      };
    }
    {
      name = "com.onepassword.OnePassword";
      text = toINI {} {
        Context = {
          # wayland disabled for now because copy-paste doesn't work.
          sockets = "x11;!wayland;fallback-x11;";
          shared = "ipc;network;";
          devices = "all";
          filesystems = concatStringsSep ";" [
            "xdg-download"
            "xdg-config/1Password"
          ];
        };
        "Session Bus Policy" = {
          "org.freedesktop.secrets" = "talk";
          "org.kde.StatusNotifierWatcher" = "talk";
          "org.freedesktop.Notifications" = "talk";
          "com.canonical.AppMenu.Register" = "talk";
        };
      };
    }
    {
      name = "com.github.micahflee.torbrowser-launcher";
      text = toINI {} {
        Context = {
          sockets = "x11;wayland;fallback-x11;";
          shared = "ipc;network;";
          devices = "dri;";
          filesystems = "xdg-download;";
        };
      };
    }
    {
      name = "com.github.tchx84.Flatseal";
      text = toINI {} {
        Context = {
          shared = "ipc;";
          sockets = "wayland;fallback-x11;";
          devices = "dri;";
          filesystems = concatStringsSep ";" [
            "xdg-data/flatpak/overrides:create"
            "xdg-data/flatpak/app:ro"
            "/var/lib/flatpak/app:ro"
            ""
          ];
        };
        "System Bus Policy" = {
          "org.freedesktop.impl.portal.PermissionStore" = "talk";
        };
      };
    }
    {
      name = "com.logseq.Logseq";
      text = toINI {} {
        Context = {
          sockets = "x11;wayland;cups;";
          shared = "network;ipc;";
          devices = "dri;";
          filesystems = "xdg-documents;";
          persistent = ".logseq;";
        };
      };
    }
    {
      name = "im.riot.Riot";
      text = toINI {} {
        Context = {
          sockets = "x11;wayland;";
          shared = "network;ipc;";
          devices = "all;";
          filesystems = "xdg-download;xdg-run/keyring;";
        };
      };
    }
    {
      name = "org.inkscape.Inkscape";
      text = toINI {} {
        Context = {
          sockets = "x11;wayland;";
          shared = "ipc;";
          filesystems = "xdg-desktop;xdg-download;xdg-pictures;";
        };
      };
    }

    {
      name = "org.fedoraproject.MediaWriter";
      text = toINI {} {
        Context = {
          sockets = "x11;wayland;";
          shared = "ipc;";
          devices = "all;";
          filesystems = concatStringsSep ";" [
            "xdg-desktop"
            "xdg-download"
            "xdg-pictures"
            "xdg-videos"
            "xdg-config/kdeglobals:ro"
            "/media"
            "/run/media"
          ];
        };
        "System Bus Policy" = {
          "org.freedesktop.UDisks2" = "talk";
        };
      };
    }
    {
      name = "org.kde.kdenlive";
      text = toINI {} {
        Context = {
          sockets = "x11;wayland;";
          shared = "ipc;";
          devices = "dri;";
          filesystems = concatStringsSep ";" [
            "xdg-desktop"
            "xdg-download"
            "xdg-pictures"
            "xdg-videos"
            "/media"
            "/run/media"
          ];
        };
      };
    }
    {
      name = "org.kde.krita";
      text = toINI {} {
        Context = {
          sockets = "x11;wayland;";
          shared = "ipc;";
          devices = "dri;";
          filesystems = "xdg-desktop;xdg-download;xdg-pictures;";
        };
      };
    }
    {
      name = "org.libreoffice.LibreOffice";
      text = toINI {} {
        Context = {
          sockets = "x11;wayland;fallback-x11;";
          shared = "ipc;";
          devices = "dri;";
          filesystems = "xdg-desktop;xdg-documents;xdg-download;";
        };
      };
    }
    {
      name = "org.musicbrainz.Picard";
      text = toINI {} {
        Context = {
          sockets = "x11;wayland;";
          shared = "network;ipc;";
          filesystems = "xdg-music;xdg-download;/tmp;";
        };
      };
    }
    {
      name = "org.remmina.Remmina";
      text = toINI {} {
        Context = {
          sockets = "x11;wayland;fallback-x11;ssh-auth;pcsc;cups;";
          shared = "network;ipc;";
          devices = "dri;all;";
          filesystems = "xdg-download;";
        };
        "System Bus Policy" = {
          "org.freedesktop.secrets" = "talk";
        };
      };
    }
    {
      name = "org.videolan.VLC";
      text = toINI {} {
        Context = {
          sockets = "x11;";
          shared = "network;ipc;";
          devices = "dri;all;";
          filesystems = concatStringsSep ";" [
            "host:ro"
            "xdg-download"
            "xdg-pictures"
            "xdg-videos"
            "/media:ro"
            "/run/media:ro"
            "/tmp"
          ];
        };
        "System Bus Policy" = {
          "org.freedesktop.secrets" = "talk";
        };
      };
    }
    {
      name = "us.zoom.Zoom";
      text = toINI {} {
        Context = {
          sockets = "x11;wayland;";
          shared = "network;ipc;";
          devices = "all;";
          filesystems = "xdg-download;!~/Documents/Zoom;!~/.zoom";
          persistent = ".zoom";
        };
      };
    }
    # end override list
  ];
  flatpak_all_overrides = symlinkJoin {
    name = "flatpak_overrides";
    paths = flatpak_overrides;
  };
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
          {
            method = "symlink";
            directory = ".var/app";
          }
        ];
      };
    };
    system.activationScripts.makeFlatpakOverrides = stringAfter ["var"] ''
      mkdir -p /var/lib/flatpak
      rm -rf /var/lib/flatpak/overrides
      ln -s ${flatpak_all_overrides} /var/lib/flatpak/overrides
    '';
    systemd = {
      services.flatpak-setup = {
        description = "Setup system Flatpak";
        after = ["network-online.target"];
        wants = ["network-online.target"];
        # this can slow down boot considerably
        # wantedBy = ["graphical.target"];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = "yes";
        };
        script = let
          flathub_cmd =
            concatStringsSep "\n"
            (map
              (x: "${pkgs.flatpak}/bin/flatpak install flathub ${x} -y --noninteractive --verbose >> ~/.cache/flatpak.log 2>&1")
              flathub_apps);
        in ''
          ${pkgs.flatpak}/bin/flatpak config --system --set languages "en"
          ${pkgs.flatpak}/bin/flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
          ${flathub_cmd}
          ${pkgs.flatpak}/bin/flatpak uninstall --system --unused -y --noninteractive

          # discord/openasar
          ${pkgs.flatpak}/bin/flatpak mask --system com.discordapp.Discord
          DISCOSAR=/var/lib/flatpak/app/com.discordapp.Discord/current/active/files/discord/resources/app.asar
          if [ -f "$DISCOSAR" ]; then
            DISCOSARSIZE=$(stat -c%s "$DISCOSAR")
            if (( DISCOSARSIZE > 1000000 )); then
              ${pkgs.curl}/bin/curl https://github.com/GooseMod/OpenAsar/releases/download/nightly/app.asar > "$DISCOSAR"
            fi
          fi
        '';
      };
    };
  };
}
