{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.gaming;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.gaming = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    security.wrappers = {
      gamescope = {
        owner = "root";
        group = "root";
        source = "${pkgs.gamescope}/bin/gamescope";
        capabilities = "cap_sys_nice+pie";
      };
    };
    boot.blacklistedKernelModules = [
      "hid-nintendo"
    ];
    networking.firewall = {
      # https://portforward.com/halo-infinite/
      allowedTCPPorts = [3074];
      allowedUDPPorts = [88 500 3074 2075 3544 4500];
    };
    programs = {
      steam = {
        enable = true;
      };
      gamemode = {
        enable = true;
        enableRenice = true;
      };
    };

    # Override Steam package to provide extra libraries for games
    nixpkgs.config.packageOverrides = pkgs: let
      # TODO(upgrade) fonts.fonts to fonts.packages once everything is on 23.11
      fontsPkg = pkgs: (pkgs.runCommand "share-fonts" {preferLocalBuild = true;} ''
        mkdir -p "$out/share/fonts"
        font_regexp='.*\.\(ttf\|ttc\|otf\|pcf\|pfa\|pfb\|bdf\)\(\.gz\)?'
        find ${toString (config.fonts.fonts)} -regex "$font_regexp" \
          -exec ln -sf -t "$out/share/fonts" '{}' \;
      '');
    in {
      steam = pkgs.steam.override {
        #withJava = true;
        extraPkgs = pkgs:
          with pkgs; [
            (fontsPkg pkgs)
            at-spi2-atk
            #binutils  # this conflicts with the gcc package
            fmodex
            gtk3
            gtk3-x11
            harfbuzz
            icu
            glxinfo
            inetutils
            keyutils
            libgdiplus
            libkrb5
            libpng
            libpulseaudio
            libthai
            libvorbis
            mono5
            pango
            stdenv.cc.cc.lib
            strace
            xorg.libXcursor
            xorg.libXi
            xorg.libXinerama
            xorg.libXScrnSaver
            zlib
            libunwind # for titanfall 2 Northstart launcher
          ];
      };
    };
    hardware = {
      steam-hardware.enable = true;

      # xone.enable = true; # xbox one wired/wireless driver
      # TODO: fork? test? try with regular xbox controller
      # xboxdrv.enable = true; # userspace xbox driver
    };
    home-manager.users."${username}" = {pkgs, ...} @ hm: {
      home.persistence."/persist/home/${username}" = mkIf withImpermanence {
        directories = [
          ".config/lutris"
          ".local/share/lutris"
          ".local/share/bottles"
          ".local/share/applications/wine"
          ".config/heroic"
          ".config/legendary"
          ".config/steamtinkerlaunch"
          # NOTE this is very important. the steam dirs must be symlinked, otherwise steam will not load properly and the system will hang
          {
            method = "symlink";
            directory = ".local/share/Steam";
          }
          {
            method = "symlink";
            directory = ".steam";
          }
        ];
      };

      home.file.".local/share/applications/steam.desktop" = {
        text = ''
          [Desktop Entry]
          Name=Steam (Clearnet)
          Comment=Application for managing and playing games on Steam
          Exec=${pkgs.mullvad-vpn}/bin/mullvad-exclude steam %U
          Icon=steam
          Terminal=false
          Type=Application
          Categories=Network;FileTransfer;Game;
          MimeType=x-scheme-handler/steam;x-scheme-handler/steamlink;
          PrefersNonDefaultGPU=true
          X-KDE-RunOnDiscreteGpu=true
        '';
      };
      home.packages = with pkgs; [
        evtest # misc input debug
        linuxConsoleTools # joystick testing
        protonup-ng # latest and greatest proton

        vkbasalt
        goverlay

        steamtinkerlaunch
        steam-run
        protontricks
        prismlauncher
        input-remapper
        heroic
        bottles
        gamescope
        lutris
        wineWowPackages.waylandFull
        winetricks
      ];
      programs = {
        mangohud = {
          enable = true;
          settings = {
            background_alpha = 0;
            cpu_color = "FFFFFF";
            cpu_temp = true;
            engine_color = "FFFFFF";
            font_size = 20;
            fps = true;
            fps_limit = "144+60+0";
            frame_timing = 0;
            gamemode = true;
            gl_vsync = 0;
            gpu_color = "FFFFFF";
            gpu_temp = true;
            no_small_font = true;
            offset_x = 50;
            position = "top-right";
            toggle_fps_limit = "Ctrl_L+Shift_L+F1";
            vsync = 1;
          };
        };
      };
    };
  };
}
