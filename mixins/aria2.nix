{pkgs, ...}: {
  config = {
    home-manager.users.ramblurr = {pkgs, ...}: {
      programs.aria2 = {
        enable = true;
        settings = {
          listen-port = "6881-6999";
          dht-listen-port = "6881-6999";
        };
      };
    };
  };
}
