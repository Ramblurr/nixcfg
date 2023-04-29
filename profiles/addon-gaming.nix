{
  pkgs,
  config,
  inputs,
  ...
}: let
  _foo = "bar";
in {
  config = {
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
    hardware = {
      # xone.enable = true; # xbox one wired/wireless driver
      # TODO: fork? test? try with regular xbox controller
      # xboxdrv.enable = true; # userspace xbox driver
    };
    home-manager.users.ramblurr = {
      pkgs,
      config,
      ...
    } @ hm: {
      home.persistence."/persist/home/ramblurr" = {
        directories = [
          {
            method = "symlink";
            directory = ".steam";
          }
          {
            method = "symlink";
            directory = ".local/share/Steam";
          }
          ".config/lutris"
          ".local/share/lutris"
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
        protontricks
        prismlauncher
        input-remapper
        heroic
        bottles
        gamescope
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
