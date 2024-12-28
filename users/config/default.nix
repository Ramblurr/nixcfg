{
  imports = [
    ../modules

    ./htop.nix
    #./impermanence.nix
    #./shell
    #./utils.nix
  ];

  xdg.configFile."nixpkgs/config.nix".text = "{ allowUnfree = true; }";
}
