{pkgs, ...}: {
  config = {
    home-manager.users.ramblurr = {
      pkgs,
      lib,
      ...
    }: {
      # note using home-manager programs.ssh because of
      # https://github.com/nix-community/home-manager/issues/322
      home.file.".ssh/control/.keep".text = "";
      home.persistence = {
        "/persist/home/ramblurr".directories = [".ssh"];
      };
    };
  };
}
