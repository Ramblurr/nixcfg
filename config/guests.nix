{ inputs, config, ... }:
{
  imports = [
    ./common-server.nix
    ./root.nix
    ./site.nix
    ./secrets.nix
    ../modules/services/sshd.nix
    ../modules/microvm-guest
    ../modules/meta.nix
    ../modules/secrets.nix
    ../modules/sops.nix
    ../modules/impermanence/default.nix
  ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    sharedModules = [
      {
        home.stateVersion = config.system.stateVersion;
      }
    ];
  };
}
