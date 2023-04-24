{
  config,
  pkgs,
  inputs,
  ...
}: {
  config = {
    home-manager.users.ramblurr = {pkgs, ...} @ hm: {
      programs.htop = {
        enable = true;
        settings = {
          hide_kernel_threads = 1;
          hide_userland_threads = 1;
        };
      };
    };
  };
}
