{ config, lib, pkgs, ... }: {
  boot.extraModprobeConfig = ''
    options snd-aloop id=RoonLoopback
  '';
  services.avahi.enable = true;
  services.roon-bridge = {
    enable = false;
    user = "ramblurr";
    group = "ramblurr";
    openFirewall = true;
  };
  # These ports are required for roon-bridge to work
  networking.firewall = {
    allowedTCPPortRanges = [
      {
        from = 9100;
        to = 9200;
      }
      {
        from = 32768;
        to = 60999;
      }
    ];
    allowedUDPPorts = [ 9003 ];
    allowedUDPPortRanges = [{
      from = 32768;
      to = 60999;
    }];
  };
  myhm = { ... }@hm: {
    #persistence = {
    #  directories = [
    #  ];
    #};
    home.file.".local/share/applications/roon.desktop" = {
      text = ''
        [Desktop Entry]
        Name=Roon
        Exec=bottles-cli run -p Roon -b 'Roon' -- %u
        Type=Application
        Terminal=false
        Categories=Application;
        Icon=/srv/data/roon-client/Roon/icons/Roon.png
        Comment=Launch Roon using Bottles.
        StartupWMClass=Roon
        Actions=Configure;
        [Desktop Action Configure]
        Name=Configure in Bottles
        Exec=bottles -b 'Roon'
      '';
    };
  };
  environment.persistence."/persist" = { directories = [ "/var/lib/roon-bridge" ]; };
}
