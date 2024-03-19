{
  config,
  pkgs,
  lib,
  inputs,
  unstable,
  ...
}: let
  hn = "fairybox";
  machine-id = "1b2d9977b3bb44508d67a72c7425828b";
  defaultSopsFile = ./secrets.sops.yaml;

  ramblurr = import ../../../ramblurr.nix {inherit config lib pkgs inputs;};
  pigpio = pkgs.callPackage ../../../../packages/pigpio {};
  pigpio-py = pkgs.python311.pkgs.callPackage ../../../../packages/pigpio-py {pigpio-c = pigpio;};
in {
  imports = [
    inputs.nixos-raspberrypi-stable.nixosModules.base
    inputs.nixos-raspberrypi-stable.nixosModules.hardware
    inputs.nixos-raspberrypi-stable.inputs.nixos-hardware.nixosModules.raspberry-pi-4
    ./audio.nix
    ./gpio.nix
    ./rpi-kernel.nix
    ../../../home-wifi.nix
  ];
  system.stateVersion = "23.11";

  environment.etc."machine-id".text = machine-id;
  sops.age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];
  sops.defaultSopsFile = defaultSopsFile;

  home.wifi.primary.enable = true;
  networking.firewall.allowedTCPPorts = [
    22 # ssh
    80 # http
    3000 # dev http
    7000 # nrepl
    7001 # portal
    7002 # dev nrepl
  ];

  system.activationScripts = {
    enableLingering = ''
      # remove all existing lingering users
      rm -r /var/lib/systemd/linger
      mkdir /var/lib/systemd/linger
      # enable for the subset of declared users
      touch /var/lib/systemd/linger/ramblurr
    '';
  };
  users.groups.gpio = {};
  users.groups.spi = {};
  users.groups.i2c = {};
  services.udev.extraRules = ''
    SUBSYSTEM=="bcm2835-gpiomem", KERNEL=="gpiomem", GROUP="gpio",MODE="0660"
    SUBSYSTEM=="bcm2711-gpiomem", KERNEL=="gpiomem", GROUP="gpio",MODE="0660"
    SUBSYSTEM=="gpio", KERNEL=="gpiochip*", ACTION=="add", RUN+="${pkgs.bash}/bin/bash -c 'chown root:gpio  /sys/class/gpio/export /sys/class/gpio/unexport ; chmod 220 /sys/class/gpio/export /sys/class/gpio/unexport'"
    SUBSYSTEM=="gpio", KERNEL=="gpio*", ACTION=="add",RUN+="${pkgs.bash}/bin/bash -c 'chown root:gpio /sys%p/active_low /sys%p/direction /sys%p/edge /sys%p/value ; chmod 660 /sys%p/active_low /sys%p/direction /sys%p/edge /sys%p/value'"
    SUBSYSTEM=="i2c-dev", GROUP="i2c",  MODE="0666"
    SUBSYSTEM=="spidev", GROUP="spi", MODE="0660"
  '';

  environment.systemPackages = with pkgs; [
    python311
    #pigpio-py
    #python311Packages.pip
    #python311Packages.spidev
    # python311Packages.rpi-gpio2
    # python311Packages.libgpiod
    #rpi-gpio2_1
    #pylibgpiod_11
    #python311Packages.pylibgpiod_11
    #python311Packages.evdev
    # gcc
    # dtc
    raspberrypifw
    gpio-utils
    pigpio
    zulu17
    vlc
  ];

  systemd.services.pigpiod = {
    enable = true;
    wantedBy = ["multi-user.target"];
    description = "pigpio daemon";
    serviceConfig = {
      Type = "forking";
      PIDFile = "pigpio.pid";
      ExecStart = "${pigpio}/bin/pigpiod -l -n 127.0.0.1 -t0";
    };
  };
  systemd.tmpfiles.rules = [
    "d /var/lib/fairybox 750 ramblurr ramblurr"
  ];

  systemd.services.fairybox = {
    enable = false;
    wantedBy = ["multi-user.target"];
    description = "fairybox";
    environment = {
      NREPL_HOST = "10.9.6.33";
      PORT = "80";
      DB_PATH = "/var/lib/fairybox/db.edn";
    };
    serviceConfig = {
      Type = "simple";
      User = "ramblurr";
      WorkingDirectory = "/var/lib/fairybox";
      ExecStart = "${pkgs.zulu17}/bin/java -XX:-OmitStackTraceInFastThrow -DPIGPIOD_HOST=127.0.0.1 -jar /home/ramblurr/box-standalone.jar";
    };
  };

  modules = {
    networking.default.enable = true;
    networking.default.hostName = hn;
    shell = {
      htop.enable = true;
      tmux.enable = true;
      zsh.enable = true;
    };
    hardware = {
      udisks2.enable = false;
      fwupd.enable = false;
    };
    services = {
      sshd.enable = true;
    };
    editors = {
      vim.enable = true;
    };
    users.enable = true;
    users.primaryUser = {
      username = ramblurr.username;
      uid = 1001;
      name = ramblurr.name;
      homeDirectory = ramblurr.homeDirectory;
      signingKey = ramblurr.signingKey;
      email = ramblurr.email;
      passwordSecretKey = ramblurr.passwordSecretKey;
      defaultSopsFile = defaultSopsFile;
      shell = pkgs.zsh;
      extraGroups = [
        "wheel"
        "gpio"
        "spi"
        "i2c"
        "audio"
      ];
    };
  };
}
