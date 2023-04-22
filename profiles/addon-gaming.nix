{
  pkgs,
  config,
  inputs,
  ...
}: let
  _foo = "bar";
in {
  config = {
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
      #gamescope = {
      #  enable = true;
      #  enableRenice = true;
      #  # settings = {};
      #};
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
      home.packages = with pkgs; [
        evtest # misc input debug
        linuxConsoleTools # joystick testing
        protonup-ng # latest and greatest proton

        vkbasalt
        goverlay

        inputs.jstest-gtk.packages.${stdenv.hostPlatform.system}.default
        inputs.xboxdrv.packages.${stdenv.hostPlatform.system}.default
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
