{pkgs, ...}: {
  config = {
    home-manager.users.ramblurr = {
      pkgs,
      lib,
      ...
    }: {
      home.file.".ssh/control/.keep".text = "";
      programs.ssh = {
        enable = true;
        controlPath = "/home/ramblurr/.ssh/control/%C";
      };

      home.persistence = {
        "/persist/home/ramblurr".directories = [".ssh"];
      };
    };
  };
}
